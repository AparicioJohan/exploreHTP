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
        open = "closed",
        bg = "white",
        title = "Help",
        helpText(
          "This panel is going to help you to visualize plot information."
        )
      ),
      fluidRow(
        h2("Inputs"),
        # column(),
        # column(),
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
          selectInput(
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
          actionButton(ns("submit"), "Submit", icon = icon("thumbs-up"))
        ),
        column(width = 6, plotOutput(ns("plot_time"), height = "600px")),
        column(width = 6, DTOutput(ns("data_table")))
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
        out <- data.table::rbindlist(out, fill = TRUE)
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
        values <- plot_shape()[, input$plot_id]
        updateSelectInput(
          session = session,
          inputId = "uid",
          choices =  values # Update choices with column names
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    w <- Waiter$new(
      html = HTML(
        "<center>",
        '<div class="dots-loader"></div>',
        "<br><br><br>",
        '<h3 style="color: grey;">Processing...</h3>',
        "</center>"
      ),
      color = transparent(0.4)
    )

    observeEvent(input$submit,
      {
        path_shape <- input$plot_shape$datapath
        path_plots <- paste0(path_plot(), "/")
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
                message = "Creating Time Serie",
                position = "bottom-right",
                showMethod = "slideDown",
                hideMethod = "hide",
                hideEasing = "linear"
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
                      color = "black"
                    )
                  )
                  gg_objt({tmp$figure})
                  dt_table({tmp$info})
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
      req(gg_objt())  # Render only if a plot is available
      gg_objt()
    })


    # Final Table
    output$data_table <- renderDT({
      req(dt_table())
      dt <- dplyr::mutate_if(dt_table(), is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top"
      )
    })
  })
}

## To be copied in the UI
# mod_03_plot_visual_ui("03_plot_visual_1")

## To be copied in the server
# mod_03_plot_visual_server("03_plot_visual_1")
