#' 03_plot_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_plot_visual_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        open = "desktop",
        bg = "white",
        title = "Visualizer",
        helpText(
          "This panel is going to help you to visualize plot information."
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("remove_border"),
          label = "Remove Bg",
          value = TRUE,
          status = "primary",
          right = TRUE
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("file_type"),
          label = "Save As:",
          choices = c("svg", "png"),
          size = "sm",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        sliderInput(
          inputId = ns("width"),
          label = "Width",
          min = 2,
          max = 15,
          value = 10,
          step = 1
        ),
        sliderInput(
          inputId = ns("height"),
          label = "Height",
          min = 2,
          max = 15,
          value = 6,
          step = 1
        )
      ),
      fluidRow(
        h2("Inputs"),
        column(
          width = 4,
          fileInput(
            inputId = ns("plot_shape"),
            label = "Output Shapefile (.gpkg)",
            accept = c(".gpkg"),
            width = "80%"
          ),
          selectInput(
            inputId = ns("plot_id"),
            label = "Column Plot:",
            choices = NULL,
            width = "80%"
          ),
          selectizeInput(
            inputId = ns("uid"),
            label = "Select Plot ID:",
            choices = NULL,
            width = "80%"
          )
        ),
        column(
          width = 4,
          br(),
          shinyDirButton(
            id = ns("directory_plots"),
            label = "Select Plot Directory",
            title = "Choose any folder",
            icon = icon("magnifying-glass"),
            style = "width: 80%"
          ),
          textOutput(ns("dirPathPlot")),
          br(),
          numericInput(
            inputId = ns("base_size"),
            label = "Base Size",
            min = 1,
            max = 30,
            value = 18,
            step = 1,
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
            inputId = ns("apply_angle"),
            label = "Apply Angle?",
            value = FALSE
          ),
          actionButton(ns("submit"), "Submit", icon = icon("thumbs-up")),
          downloadButton(outputId = ns("download"), label = "Download")
        ),
        column(width = 6, br(), plotOutput(ns("plot_time"))),
        column(width = 6, br(), DTOutput(ns("data_table")))
      )
    )
  )
}

#' 03_plot_visual Server Functions
#'
#' @noRd
mod_03_plot_visual_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    gg_objt <- reactiveVal(FALSE)
    dt_table <- reactiveVal(FALSE)
    path_plot <- reactiveVal()

    # Enable directory selection
    roots <- getVolumes()()
    shinyDirChoose(input, "directory_plots", roots = roots, session = session)

    # Path Plots
    observeEvent(input$directory_plots,
      {
        path_plot(parseDirPath(roots, input$directory_plots))
      },
      ignoreInit = TRUE
    )
    output$dirPathPlot <- renderText({
      path_plot()
    })

    # Plot Shape
    plot_shape <- reactive({
      req(input$plot_shape)
      if (!is.null(input$plot_shape)) {
        layers <- st_layers(dsn = input$plot_shape$datapath) |>
          as.data.frame() |>
          suppressWarnings()
        out <- list()
        for (i in layers$name) {
          out[[i]] <- st_read(
            dsn = input$plot_shape$datapath,
            layer = i,
            quiet = TRUE
          )
        }
        out <- do.call(what = rbind, args = out)
      } else {
        out <- NULL
      }
      return(out)
    })

    # Update Select Plot
    observeEvent(plot_shape(), {
      updateSelectInput(
        session = session,
        inputId = "plot_id",
        choices = colnames(plot_shape()) # Update choices with column names
      )
    })

    observeEvent(input$plot_id,
      {
        values <- sf::st_drop_geometry(plot_shape())[, input$plot_id]
        updateSelectizeInput(
          session = session,
          inputId = "uid",
          choices = values, # Update choices with column names
          server = TRUE
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Progress
    w <- Waiter$new(
      html = HTML(
        "<center>",
        '<div class="dots-loader"></div>',
        "<br><br><br>",
        '<h4
          style="color: white;
          background-color: #D0D0D0;
          padding: 10px;
          border-radius: 10px;">Processing...</h3>',
        "</center>"
      ),
      color = transparent(0.4)
    )

    observeEvent(input$submit,
      {
        path_shape <- input$plot_shape$datapath
        path_plots <- paste0(path_plot(), "/")
        remove_border <- input$remove_border
        color_val <- "red"
        if (input$apply_angle) angle <- input$angle else angle <- NULL
        shinyalert(
          title = "Are you sure?",
          text = "Do you want to proceed with this action?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes, proceed",
          cancelButtonText = "No, cancel",
          confirmButtonCol = "#007bc2",
          callbackR = function(x) {
            if (x) {
              # If user clicks confirm
              shinytoastr::toastr_success(
                title = "Action Confirmed!",
                message = "Creating Time Serie...",
                position = "bottom-right",
                showMethod = "slideDown",
                hideMethod = "hide",
                hideEasing = "linear",
                timeOut = 10000,
                closeButton = TRUE
              )
              w$show()
              tryCatch(
                {
                  tmp <- suppressWarnings(
                    plot_organizer(
                      id = input$uid,
                      plot_id = input$plot_id,
                      path = path_plots,
                      path_shape = path_shape,
                      base_size = input$base_size,
                      angle = angle,
                      color = "black",
                      remove_border = remove_border,
                      color_grid = color_val
                    )
                  )
                  gg_objt({
                    tmp$figure
                  })
                  dt_table({
                    tmp$info
                  })
                },
                error = function(e) {
                  shinytoastr::toastr_error(
                    title = "Error:",
                    conditionMessage(e),
                    closeButton = TRUE,
                    position = "bottom-right",
                    showMethod = "slideDown",
                    timeOut = 0
                  )
                }
              )
              w$hide()
            } else {
              # If user clicks cancel
              shinytoastr::toastr_warning(
                title = "Action Canceled!",
                message = "Please Review and Submit Again",
                position = "bottom-right",
                showMethod = "slideDown",
                hideMethod = "hide",
                hideEasing = "linear"
              )
            }
          }
        )
      },
      ignoreInit = TRUE
    )

    output$plot_time <- renderPlot({
      req(gg_objt()) # Render only if a plot is available
      gg_objt()
    })

    # Final Table
    output$data_table <- renderDT({
      req(dt_table())
      dt <- dt_table() |>
        sf::st_drop_geometry() |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top"
      )
    })

    # Download Figure
    output$download <- downloadHandler(
      filename = function() {
        req(dt_table())
        paste0("plot_", input$uid, ".", input$file_type)
      },
      content = function(file) {
        if (input$file_type == "png") {
          grDevices::png(
            filename = file,
            width = input$width,
            height = input$height,
            units = "in",
            res = 300
          )
          print(gg_objt())
          grDevices::dev.off()
        } else {
          grDevices::svg(file, width = input$width, height = input$height)
          print(gg_objt())
          grDevices::dev.off()
        }
      }
    )
  })
}

## To be copied in the UI
# mod_03_plot_visual_ui("03_plot_visual_1")

## To be copied in the server
# mod_03_plot_visual_server("03_plot_visual_1")
