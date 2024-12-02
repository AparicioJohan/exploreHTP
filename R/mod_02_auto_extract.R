#' 02_auto_extract UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_auto_extract_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        open = "closed",
        bg = "white",
        title = "Help",
        helpText(
          "This panel is going to help you to extract information from RGB
              ans DSM images."
        ),
        checkboxInput(
          inputId = ns("save_plots"),
          label = "Save Plots",
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("save_shape"),
          label = "Save Shape",
          value = TRUE
        ),
        # Checkbox for time series
        checkboxInput(
          inputId = ns("time_serie"),
          label = "Generate Time Series",
          value = TRUE
        ),
      ),
      fluidRow(
        h2("Inputs"),
        column(
          width = 12,
          shinyDirButton(
            id = ns("directory_rgb"),
            label = "Select RGB Directory",
            title = "Choose any folder",
            icon = icon("magnifying-glass")
          ),
          shinyDirButton(
            id = ns("directory_dsm"),
            label = "Select DSM Directory",
            title = "Choose any folder",
            icon = icon("magnifying-glass")
          )
        ),
        column(
          width = 12,
          textOutput(ns("dirPathRGB")),
          textOutput(ns("dirPathDSM"))
        ),
        column(
          width = 4,
          br(),
          fileInput(
            inputId = ns("plot_shape"),
            label = "Grid Shapefile (.gpkg)",
            accept = c(".gpkg"),
            width = "80%"
          ),
          actionButton(
            inputId = ns("view_shape"),
            label = "View",
            icon = icon("eye"),
            class = "btn-primary"
          ),
          br(),
          br(),
          checkboxInput(
            inputId = ns("optionals"),
            label = "More Options?",
            value = FALSE,
            width = "100%"
          ),
          conditionalPanel(
            condition = "input.optionals == true",
            ns = ns,
            fileInput(
              inputId = ns("plot_shape_crop"),
              label = "Shapefile to Crop (Optional)",
              accept = c(".gpkg"),
              width = "80%"
            ),
            fileInput(
              inputId = ns("area"),
              label = "Area of Interest (Optional)",
              accept = c(".gpkg"),
              width = "80%"
            )
          )
        ),
        column(
          width = 4,
          br(),
          selectInput(
            inputId = ns("indices"),
            label = "Select Indices:",
            choices = c(
              "BI", "BIM", "SCI", "GLI",
              "HI", "NGRDI", "SI", "VARI",
              "HUE", "BGI"
            ),
            selected = c("GLI", "NGRDI", "BGI"),
            multiple = TRUE
          ),
          selectInput(ns("plot_id"), "Select Plot ID:", choices = NULL),
          textInput(
            inputId = ns("days"),
            label = "Days (comma-separated):",
            value = "28, 42, 50, 62, 77, 84, 96, 105"
          )
        ),
        column(
          width = 4,
          br(),
          # Text input for experiment name
          textInput(
            inputId = ns("name_experiment"),
            label = "Experiment Name:",
            value = "HARS22_chips"
          ),
          shinyDirButton(
            id = ns("directory_out"),
            label = "Output Directory",
            title = "Choose any folder",
            icon = icon("magnifying-glass")
          ),
          # Submit button
          actionButton(ns("submit"), "Submit", icon = icon("thumbs-up")),
          textOutput(ns("dirPathOut"))
        ),
        column(width = 12, DTOutput(ns("data_table")))
      )
    )
  )
}

#' 02_auto_extract Server Functions
#'
#' @noRd
mod_02_auto_extract_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive Values
    path_rgb <- reactiveVal()
    path_dsm <- reactiveVal()
    path_out <- reactiveVal()
    results <- reactiveVal()

    # Area of Interest
    area_of_interest <- reactive({
      if (!is.null(input$area)) {
        out <- st_read(dsn = input$area$datapath, quiet = TRUE)
      } else {
        out <- NULL
      }
      return(out)
    })

    # Plot Shape
    plot_shape <- reactive({
      req(input$plot_shape)
      if (!is.null(input$plot_shape)) {
        out <- st_read(dsn = input$plot_shape$datapath, quiet = TRUE)
      } else {
        out <- NULL
      }
      return(out)
    })

    # Plot Shape Crop
    plot_shape_crop <- reactive({
      if (!is.null(input$plot_shape_crop)) {
        out <- st_read(dsn = input$plot_shape_crop$datapath, quiet = TRUE)
      } else {
        out <- NULL
      }
      return(out)
    })

    # Enable directory selection
    roots <- getVolumes()()
    shinyDirChoose(input, "directory_rgb", roots = roots, session = session)
    shinyDirChoose(input, "directory_dsm", roots = roots, session = session)
    shinyDirChoose(input, "directory_out", roots = roots, session = session)

    # Path RGB
    observeEvent(input$directory_rgb,
      {
        path_rgb(parseDirPath(roots, input$directory_rgb))
      },
      ignoreInit = TRUE
    )
    output$dirPathRGB <- renderText({
      path_rgb()
    })

    # Path DSM
    observeEvent(input$directory_dsm,
      {
        path_dsm(parseDirPath(roots, input$directory_dsm))
      },
      ignoreInit = TRUE
    )
    output$dirPathDSM <- renderText({
      path_dsm()
    })

    # Path Output
    observeEvent(input$directory_out,
      {
        path_out(parseDirPath(roots, input$directory_out))
      },
      ignoreInit = TRUE
    )
    output$dirPathOut <- renderText({
      path_out()
    })

    # Update Select Plot
    observeEvent(plot_shape(), {
      updateSelectInput(
        session = session,
        inputId = "plot_id",
        choices = colnames(plot_shape()) # Update choices with column names
      )
    })

    # Progress
    w <- Waiter$new(
      html = HTML("<center> <div class='ball-loader'></div> </center>"),
      color = transparent(0.4)
    )
    update_progress <- function(current_step, total_steps) {
      w$update(
        html = HTML(
          "<center>",
          '<div class="dots-loader"></div>',
          "<br><br><br>",
          sprintf(
            '<h3 style="color: grey;">Processing step %d of %d</h3>',
            current_step, total_steps
          ),
          "</center>"
        )
      )
    }

    # Run Extraction
    observeEvent(input$submit,
      {
        path_rgb <- path_rgb()
        path_dsm <- path_dsm()
        path_out <- path_out()
        indices <- input$indices
        bands <- c("Red", "Green", "Blue")
        dap <- as.numeric(unlist(strsplit(input$days, ",\\s*")))
        plot_id <- input$plot_id
        save_plots <- input$save_plots
        save_shape <- input$save_shape
        time_serie <- input$time_serie
        name_experiment <- input$name_experiment
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
                message = "Starting Extraction",
                position = "bottom-right",
                showMethod = "slideDown",
                hideMethod = "hide",
                hideEasing = "linear"
              )
              w$show()
              tryCatch(
                {
                  results({
                    auto_extract(
                      path_rgb = path_rgb,
                      path_dsm = path_dsm,
                      area_of_interest = area_of_interest(),
                      plot_shape = plot_shape(),
                      plot_shape_crop = plot_shape_crop(),
                      indices = indices,
                      bands = bands,
                      dap = dap,
                      plot_id = plot_id,
                      save_plots = save_plots,
                      save_shape = save_shape,
                      time_serie = time_serie,
                      name_experiment = name_experiment,
                      path_out = path_out,
                      update_progress = update_progress
                    )
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

    # Notification when finished
    observeEvent(results(), {
      if (!is.null(results())) {
        shinytoastr::toastr_info(
          title = "Outputs!",
          message = paste0(
            "Your files were saved in here: \n \n", path_out()
          ),
          closeButton = TRUE,
          position = "bottom-right",
          showMethod = "slideDown",
          timeOut = 0
        )
      }
    })

    # View Shape
    output$map <- renderLeaflet({
      req(plot_shape())
      plot_shape <- plot_shape()
      map <- mapview(
        x = plot_shape,
        alpha.regions = 0.5,
        aplha = 1,
        zcol = input$color_by
      )
      map@map
    })

    # Modal
    observeEvent(input$view_shape, {
      req(plot_shape())
      showModal(modalDialog(
        title = tagList(icon = icon("table-cells"), "View Shape"),
        size = "l",
        easyClose = TRUE,
        shinyWidgets::pickerInput(
          inputId = ns("color_by"),
          label = "Select Column:",
          choices = colnames(plot_shape()),
          options = shinyWidgets::pickerOptions(
            container = "body",
            style = "btn-outline-secondary"
          )
        ),
        leafletOutput(outputId = ns("map"))
      ))
    })

    # Final Table
    output$data_table <- renderDT({
      req(results())
      dt <- dplyr::mutate_if(results()$dt, is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top"
      )
    })
  })
}

## To be copied in the UI
# mod_02_auto_extract_ui("02_auto_extract_1")

## To be copied in the server
# mod_02_auto_extract_server("02_auto_extract_1")
