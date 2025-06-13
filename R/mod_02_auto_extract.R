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
  rgb_list <- exploreHTP::indices[exploreHTP::indices$band %in% "C", ]$index
  mts_list <- exploreHTP::indices[!exploreHTP::indices$band %in% "C", ]$index
  tagList(
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        open = "desktop",
        title = "Autoextract",
        shinyWidgets::materialSwitch(
          inputId = ns("save_plots"),
          label = "Save Plots",
          value = FALSE,
          status = "primary",
          right = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("save_masked_plots"),
          label = "Save Masked Plots",
          value = FALSE,
          status = "primary",
          right = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("time_serie"),
          label = "Save Time Series",
          value = FALSE,
          status = "primary",
          right = TRUE
        ),
        accordion(
          open = TRUE,
          accordion_panel(
            "Settings",
            icon = icon("cog"),
            selectInput(
              inputId = ns("seg_index"),
              label = "Segmentation Index:",
              choices = list("RGB" = rgb_list, "Multispectral" = mts_list),
              selected = c("HUE"),
              multiple = FALSE,
              width = "90%"
            ),
            checkboxInput(
              inputId = ns("mask_above"),
              label = "Remove above?",
              value = TRUE,
              width = "100%"
            ),
            textInput(
              inputId = ns("thrsh"),
              label = "Threshold:",
              value = 0,
              width = "90%"
            ),
            selectInput(
              inputId = ns("no_mask_index"),
              label = "VI without mask:",
              choices = list("None", "RGB" = rgb_list, "Multispectral" = mts_list),
              selected = c("None"),
              multiple = FALSE,
              width = "90%"
            )
          )
        )
      ),
      fluidRow(
        h2("Inputs"),
        column(
          width = 12,
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            fileInput(
              inputId = ns("plot_shape"),
              label = "Grid Shapefile (.gpkg)",
              accept = c(".gpkg"),
              width = "23%",
              placeholder = "Grid Shapefile (.gpkg)"
            ),
            actionButton(
              inputId = ns("view_shape"),
              icon = icon("eye"),
              label = NULL,
              class = "btn-primary"
            ),
            shinyDirButton(
              id = ns("directory_rgb"),
              label = "Images Directory",
              title = "Choose any folder",
              icon = icon("magnifying-glass")
            ),
            shinyWidgets::dropdownButton(
              tags$strong("Subset Images"),
              circle = FALSE,
              label = "Subset",
              icon = icon("filter"),
              width = "400px",
              margin = "20px",
              br(),
              shinyWidgets::pickerInput(
                inputId = ns("subset_img"),
                label = NULL,
                choices = NULL,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  container = "body",
                  actionsBox = TRUE,
                  size = 5
                ),
                width = "100%"
              ),
              tooltip = shinyWidgets::tooltipOptions(
                title = "Click to see more options!"
              )
            ),
            shinyWidgets::dropdownButton(
              tags$strong("Optional Inputs"),
              circle = FALSE,
              label = "More",
              icon = icon("plus"),
              width = "400px",
              margin = "20px",
              br(),
              shinyDirButton(
                id = ns("directory_dsm"),
                label = "DSM Directory",
                title = "Choose any folder",
                icon = icon("magnifying-glass"),
                style = "margin-bottom: 25px;"
              ),
              fileInput(
                inputId = ns("plot_shape_crop"),
                label = helpText("Shapefile to Crop (Optional)"),
                accept = c(".gpkg"),
                width = "100%"
              ),
              fileInput(
                inputId = ns("area"),
                label = helpText("Area of Interest (Optional)"),
                accept = c(".gpkg"),
                width = "100%"
              ),
              tooltip = shinyWidgets::tooltipOptions(
                title = "Click to see more options!"
              )
            )
          )
        ),
        column(
          width = 12,
          uiOutput(ns("dirPathRGB"))
        ),
        column(
          width = 4,
          textInput(
            inputId = ns("rgb_bands"),
            label = "RGB/RedEdge/NIR:",
            value = "1, 2, 3",
            width = "90%"
          ),
          selectInput(
            inputId = ns("indices"),
            label = tagList(
              "Select Indices:",
              actionLink(
                inputId = ns("view_indices"),
                icon = icon("eye"),
                label = "View"
              )
            ),
            choices = list("RGB" = rgb_list, "Multispectral" = mts_list),
            selected = c("GLI", "NGRDI", "BGI"),
            multiple = TRUE,
            width = "90%"
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("plot_id"),
            label = "Select Plot ID:",
            choices = NULL,
            width = "90%"
          ),
          textInput(
            inputId = ns("days"),
            label = "Days (comma-separated):",
            value = "",
            placeholder = "28, 42, 50, ...",
            width = "90%"
          )
        ),
        column(
          width = 4,
          # Text input for experiment name
          textInput(
            inputId = ns("trial_name"),
            label = "Experiment Name:",
            value = "HARS22_chips",
            width = "90%"
          ),
          shinyDirButton(
            id = ns("directory_out"),
            label = "Output Directory",
            title = "Choose any folder",
            icon = icon("magnifying-glass")
          ),
          # Submit button
          actionButton(ns("submit"), "Submit", icon = icon("thumbs-up")),
          uiOutput(ns("dirPathOut"))
        ),
        column(width = 12, br(), uiOutput(ns("ui_plot")))
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

    # Table of indices
    output$table_indices <- renderDT({
      dt <- exploreHTP::indices |>
        dplyr::rename(Index = index, Equation = eq, Band = band)
      datatable(
        data = dt,
        options = list(pageLength = 5, autoWidth = FALSE),
        filter = "top"
      )
    })

    observeEvent(input$view_indices, {
      showModal(modalDialog(
        title = tagList(icon = icon("table-cells"), "View Indices"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        DTOutput(ns("table_indices"))
      ))
    })

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
    output$dirPathRGB <- renderUI({
      tagList(
        fluidRow(
          helpText(paste0(path_rgb())),
          helpText(paste0(path_dsm()))
        )
      )
    })
    observeEvent(input$directory_rgb,
      {
        subset <- path_rgb()
        subset <- list.files(subset, pattern = "\\.tif$", full.names = TRUE)
        total_imgs <- length(subset)
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "subset_img",
          choices = 1:total_imgs,
          selected = 1:total_imgs
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(input$subset_img,
      {
        updateTextInput(
          session = session,
          inputId = "days",
          value = paste(1:length(input$subset_img), collapse = ", "),
        )
      },
      ignoreInit = TRUE
    )

    # Path DSM
    observeEvent(input$directory_dsm,
      {
        path_dsm(parseDirPath(roots, input$directory_dsm))
      },
      ignoreInit = TRUE
    )

    # Path Output
    observeEvent(input$directory_out,
      {
        path_out(parseDirPath(roots, input$directory_out))
      },
      ignoreInit = TRUE
    )
    output$dirPathOut <- renderUI({
      tagList(
        fluidRow(
          helpText(paste0(path_out(), "/", input$trial_name))
        )
      )
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
          '<div style="text-align: center;">',
          '<div class="dots-loader"></div>',
          "<br><br><br><br>",
          sprintf(
            '<h4
            style="color: white;
            background-color: #D0D0D0;
            padding: 10px;
            border-radius: 10px;"> Processing step %d of %d</h3>',
            current_step, total_steps
          ),
          "</div>"
        )
      )
    }

    # Run Extraction
    observeEvent(input$submit,
      {
        path_rgb <- if (length(path_rgb()) == 0) NULL else path_rgb()
        path_dsm <- if (length(path_dsm()) == 0) NULL else path_dsm()
        path_out <- path_out()
        indices <- input$indices
        index_mask <- if (input$seg_index == "") NULL else input$seg_index
        mask_above <- input$mask_above
        threshold <- input$thrsh
        time <- as.numeric(unlist(strsplit(input$days, ",\\s*")))
        bands <- list_bands(input$rgb_bands)
        red <- bands$red
        green <- bands$green
        blue <- bands$blue
        rededge <- bands$rededge
        nir <- bands$nir
        index_no_mask <- if (input$no_mask_index == "None") NULL else input$no_mask_index
        plot_id <- input$plot_id
        save_plots <- input$save_plots
        save_masked_plots <- input$save_masked_plots
        time_serie <- input$time_serie
        trial_name <- input$trial_name
        subset <- as.numeric(input$subset_img)
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
                message = "Please wait ...",
                position = "bottom-right",
                showMethod = "slideDown",
                hideMethod = "hide",
                hideEasing = "linear"
              )
              # w$show()
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
                      index_mask = index_mask,
                      mask_above = mask_above,
                      threshold = threshold,
                      time = time,
                      plot_id = plot_id,
                      save_plots = save_plots,
                      save_masked_plots = save_masked_plots,
                      time_serie = time_serie,
                      trial_name = trial_name,
                      path_out = path_out,
                      subset = subset,
                      red = red,
                      green = green,
                      blue = blue,
                      rededge = rededge,
                      nir = nir,
                      index_no_mask = index_no_mask
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
              # w$hide()
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
      req(input$plot_shape)
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
        footer = NULL,
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

    # UI
    output$ui_plot <- renderUI({
      req(results())
      card(
        full_screen = TRUE,
        card_header(tagList(icon = icon("chart-line"), "Data")),
        DTOutput(ns("data_table"))
      )
    })
  })
}

## To be copied in the UI
# mod_02_auto_extract_ui("02_auto_extract_1")

## To be copied in the server
# mod_02_auto_extract_server("02_auto_extract_1")
