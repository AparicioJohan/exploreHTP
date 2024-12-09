#' 04_flexfitr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_flexfitr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        open = "desktop",
        title = "flexFitR",
        accordion(
          multiple = FALSE,
          accordion_panel(
            "Parameters",
            icon = icon("cog"),
            open = TRUE,
            uiOutput(ns("components"))
          ),
          accordion_panel(
            "Settings",
            icon = icon("sliders"),
            checkboxInput(
              inputId = ns("add_zero"),
              label = "Add Zero?",
              value = FALSE,
              width = "100%"
            ),
            checkboxInput(
              inputId = ns("truncate"),
              label = "Truncate Maximum?",
              value = FALSE,
              width = "100%"
            ),
            checkboxInput(
              inputId = ns("negative"),
              label = "Check Negatives?",
              value = FALSE,
              width = "100%"
            ),
            shinyWidgets::materialSwitch(
              inputId = ns("apply_filter"),
              label = "Apply Filter?",
              value = FALSE,
              status = "primary",
              right = TRUE
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.apply_filter == true",
              sliderInput(
                inputId = ns("range"),
                label = "Filter Time",
                min = 0,
                max = 100,
                value = c(20, 70)
              )
            ),
            shinyWidgets::materialSwitch(
              inputId = ns("parallel"),
              label = "Parallel",
              value = FALSE,
              status = "primary",
              right = TRUE
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.parallel == true",
              numericInput(
                inputId = ns("n_cores"),
                label = "Choose Cores:",
                min = 1,
                max = 6,
                value = 1,
                step = 1,
                width = "80%"
              )
            )
          )
        )
      ),
      fluidRow(
        h2("Inputs"),
        column(
          width = 4,
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            fileInput(
              inputId = ns("file_extract"),
              label = "Choose File (.csv)",
              accept = c(".csv"),
              width = "70%"
            ),
            actionButton(
              ns("view_file"),
              icon = icon("eye"),
              label = NULL,
              class = "btn-primary"
            )
          ),
          selectInput(
            inputId = ns("select_x"),
            label = "Select x-variable:",
            choices = NULL,
            multiple = FALSE,
            width = "90%"
          ),
          selectInput(
            inputId = ns("select_y"),
            label = "Select y-variable:",
            choices = c("Canopy", "GLI"),
            selected = c("Canopy"),
            multiple = FALSE,
            width = "90%"
          )
        ),
        column(
          width = 4,
          br(),
          selectInput(
            inputId = ns("functions"),
            label = "Regression Function:",
            choices = list_funs(),
            selected = c("fn_liner_sat"),
            multiple = FALSE,
            width = "90%"
          ),
          selectInput(
            inputId = ns("methods"),
            label = "Optimization Methods",
            choices = list_methods(),
            selected = c("subplex"),
            multiple = TRUE,
            width = "90%"
          ),
          selectInput(
            inputId = ns("metadata"),
            label = "Metadata (Optional):",
            choices = NULL,
            multiple = TRUE,
            width = "90%"
          )
        ),
        column(
          width = 4,
          br(),
          selectInput(
            inputId = ns("group"),
            label = "Grouping (Optional):",
            choices = NULL,
            width = "90%",
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = ns("uid"),
            label = "Subset Group:",
            choices = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              container = "body",
              actionsBox = TRUE,
              size = 5,
              liveSearch = TRUE
            ),
            width = "95%"
          ),
          # Submit button
          actionButton(ns("submit"), "Submit", icon = icon("thumbs-up")),
          actionButton(ns("summary"), "Summary", icon = icon("eye"))
        ),
        column(width = 12, br(), uiOutput(ns("ui_plot")))
      )
    )
  )
}

#' 04_flexfitr Server Functions
#'
#' @noRd
mod_04_flexfitr_server <- function(id, dark_mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output_model <- reactiveVal()
    uid_active <- reactiveVal()

    # Plot Shape
    dt_reactive <- reactive({
      if (!is.null(input$file_extract)) {
        out <- read.csv(file = input$file_extract$datapath)
      } else {
        out <- flexFitR::dt_potato
      }
      return(out)
    })

    observeEvent(dt_reactive(), {
      updateSelectInput(
        session = session,
        inputId = "select_x",
        choices = colnames(dt_reactive()),
        selected = "DAP"
      )
      updateSelectInput(
        session = session,
        inputId = "select_y",
        choices = colnames(dt_reactive()),
        selected = "Canopy"
      )
      updateSelectInput(
        session = session,
        inputId = "group",
        choices = colnames(dt_reactive()),
        selected = "Plot"
      )
      updateSelectInput(
        session = session,
        inputId = "metadata",
        choices = colnames(dt_reactive()),
        selected = "gid"
      )
    })

    observe({
      req(input$group)
      req(dt_reactive())
      dt <- dt_reactive()
      if (!is.null(input$group) | input$group != "") {
        values <- unique(dt[[input$group]])
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "uid",
          choices = values,
          selected = values[1]
        )
      }
    })

    observeEvent(input$select_x,
      {
        req(dt_reactive())
        dt <- dt_reactive()
        if (!is.null(input$select_x) | input$select_x != "") {
          values <- unique(dt[[input$select_x]])
          if (is.numeric(values)) {
            min <- min(values, na.rm = TRUE)
            max <- max(values, na.rm = TRUE)
            min_lim <- min - 2 * min
            max_lim <- max + 2 * max
            updateSliderInput(
              session = session,
              inputId = "range",
              label = "Filter Time",
              min = min_lim,
              max = max_lim,
              value = c(min, max),
              step = 0.1
            )
          }
        }
      },
      ignoreInit = TRUE
    )

    output$components <- renderUI({
      fn <- input$functions
      args <- try(expr = names(formals(fn))[-1], silent = TRUE)
      tagList(
        textInput(
          inputId = ns("inital_values"),
          label = strong("Starting Values:"),
          value = paste(rep(0.1, length(args)), collapse = ", "),
          width = "100%"
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          strong("Parameters:"),
          em(paste0(args, collapse = ", "))
        ),
        hr(),
        textInput(
          inputId = ns("upper_limit"),
          label = "Upper Limit (Optional):",
          value = "-Inf",
          width = "100%"
        ),
        textInput(
          inputId = ns("lower_limit"),
          label = "Lower Limit (Optional):",
          value = "+Inf",
          width = "100%"
        )
      )
    }) |>
      bindEvent(input$functions, dt_reactive())

    # Modal Data
    output$data_table <- renderDT({
      req(dt_reactive())
      dt <- dt_reactive() |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top"
      )
    })
    observeEvent(input$view_file, {
      req(dt_reactive())
      showModal(modalDialog(
        title = tagList(icon = icon("table-cells"), "View Data"),
        size = "l",
        easyClose = TRUE,
        DTOutput(ns("data_table"))
      ))
    })

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

    # Modeling
    observeEvent(input$submit,
      {
        req(dt_reactive())
        req(input$inital_values)
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
                message = "Fitting Models...",
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
                  data <- dt_reactive() |>
                    flexFitR:::transform(
                      x = input$select_x,
                      y = input$select_y,
                      grp = input$group,
                      metadata = input$metadata,
                      check_negative = input$negative,
                      add_zero = input$add_zero,
                      max_as_last = input$truncate,
                      interval = if (input$apply_filter) input$range else NULL
                    )
                  x_var <- input$select_x
                  y_var <- input$select_y
                  if (!is.numeric(data[[x_var]])) {
                    stop("X variable must be numeric")
                  }
                  if (!is.numeric(data[[y_var]])) {
                    stop("Y variable must be numeric")
                  }
                  grp <- input$group
                  keep <- input$metadata
                  fn <- input$functions
                  parameters <- unlist(strsplit(input$inital_values, ",\\s*"))
                  parameters <- as.numeric(parameters)
                  names(parameters) <- names(formals(fn))[-1]
                  lower <- unlist(strsplit(input$lower_limit, ",\\s*"))
                  lower <- as.numeric(lower)
                  upper <- unlist(strsplit(input$upper_limit, ",\\s*"))
                  upper <- as.numeric(upper)
                  fixed_params <- NULL
                  method <- input$methods
                  subset <- input$uid
                  options <- flexFitR:::modeler.options()
                  control <- list()
                  output_model({
                    suppressWarnings(
                      modeler(
                        data = data,
                        x = x_var,
                        y = y_var,
                        grp = grp,
                        keep = keep,
                        fn = fn,
                        parameters = parameters,
                        lower = lower,
                        upper = upper,
                        fixed_params = fixed_params,
                        method = method,
                        subset = subset,
                        options = options,
                        control = control
                      )
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

    # plot time series
    output$plot_serie <- renderPlotly({
      req(output_model())
      req(input$uid_plot)
      if (!req(input$uid_plot) %in% output_model()[["param"]]$uid) {
        return()
      }
      common <- if (dark_mode() == "dark") "white" else "#1D1F21"
      obj <- plot(
        x = output_model(),
        id = input$uid_plot,
        type = 1,
        color = "#007bc2",
        color_points = common,
        title = paste("Group:", input$uid_plot)
      ) +
        theme(
          plot.title = element_text(colour = common),
          axis.title.x = element_text(colour = common),
          axis.text.x = element_text(colour = common),
          axis.title.y = element_text(colour = common),
          axis.text.y = element_text(colour = common),
          strip.text = element_text(colour = common)
        )
      plotly::ggplotly(obj) |>
        plotly::config(displayModeBar = FALSE) |>
        plotly::layout(
          font = list(family = "Open Sans", color = common),
          plot_bgcolor = "transparent",
          paper_bgcolor = "transparent"
        )
    })

    # Imported Table
    output$data_table <- renderDT({
      req(dt_reactive())
      dt <- dt_reactive() |>
        sf::st_drop_geometry() |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top"
      )
    })

    # results Table
    output$results_table <- renderDT({
      req(output_model())
      dt <- output_model()[["param"]] |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top",
        fillContainer = TRUE
      )
    })

    # Metrics Table
    output$metrics_table <- renderDT({
      req(output_model())
      dt <- metrics(output_model()) |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top",
        fillContainer = TRUE
      )
    })

    # Coefficients Table
    output$coef_table <- renderDT({
      req(output_model())
      dt <- coef(output_model()) |>
        dplyr::mutate_if(is.numeric, round, 3)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = TRUE),
        filter = "top",
        fillContainer = TRUE
      )
    })

    # Summary Model
    output$summ_model <- renderPrint({
      req(output_model())
      print(output_model())
    })

    # Summary
    observeEvent(input$summary, {
      req(output_model())
      showModal(modalDialog(
        title = tagList(icon = icon("list"), "Summary"),
        size = "l",
        easyClose = TRUE,
        verbatimTextOutput(outputId = ns("summ_model"))
      ))
    })

    observeEvent(output_model(), {
      req(output_model())
      out <- output_model()
      values <- unique(output_model()[["param"]]$uid)
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "uid_plot",
        choices = values,
        selected = values[1]
      )
    })

    # UI for plotting results
    output$ui_plot <- renderUI({
      req(output_model())
      layout_column_wrap(
        width = 1 / 2,
        card(
          full_screen = TRUE,
          card_header("Time Serie"),
          layout_sidebar(
            fillable = TRUE,
            sidebar = sidebar(
              position = "right",
              width = 220,
              open = "closed",
              shinyWidgets::pickerInput(
                inputId = ns("uid_plot"),
                label = "Group:",
                choices = NULL,
                multiple = FALSE,
                options = shinyWidgets::pickerOptions(
                  container = "body",
                  actionsBox = TRUE,
                  size = 5,
                  liveSearch = TRUE
                ),
                width = "100%"
              )
            ),
            plotlyOutput(ns("plot_serie"))
          )
        ),
        navset_card_tab(
          full_screen = TRUE,
          title = "Tables",
          nav_panel(
            "Results",
            card_body(DTOutput(ns("results_table")))
          ),
          nav_panel(
            "Coefficients",
            card_body(DTOutput(ns("coef_table")))
          ),
          nav_panel(
            "Metrics",
            card_body(DTOutput(ns("metrics_table")))
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_04_flexfitr_ui("04_flexfitr_1")

## To be copied in the server
# mod_04_flexfitr_server("04_flexfitr_1")
