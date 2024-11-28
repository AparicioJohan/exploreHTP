#' 01_grid_resize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_grid_resize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        fileInput(
          inputId = ns("shapefile"),
          label = "Choose a Simple Feature (.gpkg) File",
          accept = c(".gpkg"),
          width = "80%"
        ),
        fileInput(
          inputId = ns("rasterFile"),
          label = "Choose a .tif Raster File (Optional)",
          accept = ".tif",
          width = "80%"
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns("x"),
          label = "Choose Width (x-dim)",
          min = 0,
          max = 6,
          value = 0.8,
          step = 0.1,
          width = "80%"
        ),
        numericInput(
          inputId = ns("y"),
          label = "Choose Length (y-dim)",
          min = 0,
          max = 8,
          value = 6,
          step = 0.1,
          width = "80%"
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns("angle"),
          label = "Rotation Angle (degrees)",
          min = -180,
          max = 180,
          value = 0,
          step = 0.5,
          width = "80%"
        ),
        checkboxInput(
          inputId = ns("interactive"),
          label = "Interactive?",
          value = FALSE,
          width = "80%"
        ),
        downloadButton(ns("download"))
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = "input.interactive == true",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = ns("pixels"),
                label = "Pixels",
                min = 0,
                max = 1000000,
                value = 100000,
                step = 100
              ),
              helpText("Use only when a raster is imported.")
            ),
            column(
              width = 10,
              leafletOutput(outputId = ns("map2"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.interactive == false",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              checkboxInput(
                inputId = ns("original"),
                label = "See Original",
                value = TRUE
              ),
              checkboxInput(
                inputId = ns("new"),
                label = "See New",
                value = TRUE
              )
            ),
            column(
              width = 2,
              plotOutput(
                outputId = ns("map"),
                width = "1000px",
                height = "800px"
              )
            )
          )
        )
      )
    ),
    hr(),
    br(),
    tags$footer(
      strong("UW - Madison 2024"), # strong() = bold
      align = "center",
      style =
        "position:fixed;
         bottom:11.5px;
         width: 20%;
         height:20px;
         color: black;
         padding: 0px;
         z-index: 100;"
    )
  )
}

#' 01_grid_resize Server Functions
#'
#' @noRd
mod_01_grid_resize_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store raster and simple feature data
    rasterData <- reactiveVal()
    simpleFeature <- reactiveVal()
    simpleFeature_R <- reactiveVal()

    # Observe when raster file is uploaded
    observeEvent(input$rasterFile, {
      req(input$rasterFile)
      rasterData(rast(input$rasterFile$datapath))
    })

    # Observe when shapefile is uploaded
    observeEvent(input$shapefile, {
      req(input$shapefile)
      simpleFeature(st_read(input$shapefile$datapath, quiet = TRUE))
    })

    # Rotate geometry based on input angle
    observe({
      req(simpleFeature())
      req(input$angle)
      req(input$x)
      req(input$y)
      # Change dimension
      rotated_geom <- resize(
        plot_shape = simpleFeature(),
        mosaic = rasterData(),
        angle = input$angle,
        xsize = input$x,
        ysize = input$y
      )
      # Update the rotated simple feature
      simpleFeature_R(rotated_geom)
    })

    # Render map with raster and (rotated) simple feature
    output$map <- renderPlot({
      req(simpleFeature())
      if (is.null(rasterData())) {
        plot(simpleFeature()$geom)
      } else {
        plotRGB(rasterData())
      }
      if (input$original) {
        plot(simpleFeature()$geom, add = TRUE, col = "red")
      }
      if (input$new) {
        plot(simpleFeature_R()$geom, add = TRUE, col = "blue")
      }
    })

    # Interactive Map
    output$map2 <- renderLeaflet({
      req(simpleFeature(), simpleFeature_R())
      plot_shape <- simpleFeature()
      new_shape <- simpleFeature_R()
      map <- mapview(
        x = list(plot_shape, new_shape),
        layer.name = c("Original", "New"),
        alpha.regions = 0.5,
        aplha = 1,
        col.regions = c("blue", "red")
      )
      if (!is.null(rasterData())) {
        map <- viewRGB(
          x = as(rasterData(), "Raster"),
          layer.name = "base",
          r = 1,
          g = 2,
          b = 3,
          na.color = "#00000000",
          maxpixels = input$pixels,
          quantiles = c(0, 1)
        ) + map
      }
      map@map
    })

    # Download Shape File
    output$download <- downloadHandler(
      filename = function() {
        paste0("shapefile_", Sys.Date(), ".gpkg")
      },
      content = function(file) {
        st_write(simpleFeature_R(), file, quiet = TRUE)
      }
    )
  })
}

## To be copied in the UI
# mod_01_grid_resize_ui("01_grid_resize_1")

## To be copied in the server
# mod_01_grid_resize_server("01_grid_resize_1")
