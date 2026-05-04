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
    cicerone::use_cicerone(),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        open = "desktop",
        title = NULL,
        actionButton(
          inputId = ns("start_autoextract_tour"),
          label = "Guide",
          icon = icon("route"),
          class = "btn-outline-primary"
        ),
        tags$div(
          id = ns("guide_output_options"),
          style = "margin-top: 4px; margin-bottom: 4px;",
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
          conditionalPanel(
            ns = ns,
            condition = "input.time_serie == true",
            textInput(
              inputId = ns("angle"),
              label = "Angle:",
              value = "auto",
              width = "90%",
              placeholder = "auto"
            ),
          )
        ),
        accordion(
          open = TRUE,
          accordion_panel(
            "Settings",
            icon = icon("cog"),
            tags$div(
              id = ns("guide_segmentation_settings"),
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
        )
      ),
      fluidRow(
        # Hero Banner
        div(
          class = "hero-banner rounded-4 px-4 py-3 mb-3",
          div(
            class = "d-flex align-items-center gap-3",
            div(class = "icon-badge icon-green flex-shrink-0", icon("images")),
            div(
              h4("Autoextract", class = "fw-bold mb-0"),
              p(
                "Extract plot-level phenotypes from RGB and multispectral UAV imagery.",
                class = "text-muted small mb-0"
              )
            )
          )
        ),
        column(
          width = 12,
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$div(
              id = ns("guide_plot_shape"),
              fileInput(
                inputId = ns("plot_shape"),
                label = "Grid Shapefile (.gpkg)",
                accept = c(".gpkg"),
                width = "100%",
                placeholder = "Grid Shapefile (.gpkg)"
              )
            ),
            tags$div(
              id = ns("guide_view_shape"),
              actionButton(
                inputId = ns("view_shape"),
                icon = icon("eye"),
                label = NULL,
                class = "btn-primary"
              )
            ),
            tags$div(
              id = ns("guide_directory_rgb"),
              shinyDirButton(
                id = ns("directory_rgb"),
                label = "Images Directory",
                title = "Choose any folder",
                icon = icon("magnifying-glass")
              )
            ),
            tags$div(
              id = ns("guide_subset_img"),
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
                )
              )
            ),
            tags$div(
              id = ns("guide_optional_inputs"),
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
                )
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
          tags$div(
            id = ns("guide_rgb_bands"),
            textInput(
              inputId = ns("rgb_bands"),
              label = "RGB/RedEdge/NIR:",
              value = "1, 2, 3",
              width = "90%"
            )
          ),
          tags$div(
            id = ns("guide_indices"),
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
          )
        ),
        column(
          width = 4,
          tags$div(
            id = ns("guide_plot_id"),
            selectInput(
              inputId = ns("plot_id"),
              label = "Select Plot ID:",
              choices = NULL,
              width = "90%"
            )
          ),
          tags$div(
            id = ns("guide_days"),
            textInput(
              inputId = ns("days"),
              label = "Days (comma-separated):",
              value = "",
              placeholder = "28, 42, 50, ...",
              width = "90%"
            )
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
          tags$div(
            id = ns("guide_output"),
            shinyDirButton(
              id = ns("directory_out"),
              label = "Output Directory",
              title = "Choose any folder",
              icon = icon("magnifying-glass")
            ),
            actionButton(ns("submit"), "Submit", icon = icon("thumbs-up"))
          ),
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

    guide <- cicerone::Cicerone$
      new()$
      step(
      el = ns("guide_plot_shape"),
      title = "1. Upload plot grid",
      description = "Start by uploading the plot grid shapefile as a .gpkg file. This file defines the plot boundaries used for extraction."
    )$
      step(
      el = ns("guide_view_shape"),
      title = "2. Preview and subset grid",
      description = "Use this button to preview the shapefile. You can color plots by a column and subset plots by one or more levels before extraction."
    )$
      step(
      el = ns("guide_directory_rgb"),
      title = "3. Select image folder",
      description = "Choose the folder containing the RGB or multispectral GeoTIFF images."
    )$
      step(
      el = ns("guide_subset_img"),
      title = "4. Subset images",
      description = "Use this option if you only want to process a subset of the images in the selected folder."
    )$
      step(
      el = ns("guide_optional_inputs"),
      title = "Optional inputs",
      description = paste(
        "Use this menu to provide optional files.",
        "Add a DSM directory if you want to calculate plant height or volume.",
        "You can also provide a shapefile for cropping saved plot images",
        "or an area of interest to crop the mosaics before extraction."
      )
    )$
      step(
      el = ns("guide_rgb_bands"),
      title = "5. Define image bands",
      description = "Specify the band order. For RGB images, use red, green, and blue. For multispectral images, include red edge and NIR if needed."
    )$
      step(
      el = ns("guide_indices"),
      title = "6. Select vegetation indices",
      description = "Choose the vegetation indices to calculate for each plot."
    )$
      step(
      el = ns("guide_plot_id"),
      title = "7. Select plot ID",
      description = "Choose the shapefile column that uniquely identifies each plot."
    )$
      step(
      el = ns("guide_days"),
      title = "8. Check image times",
      description = "These values should match the selected images. For example: 28, 42, 50."
    )$
      step(
      el = ns("guide_output"),
      title = "9. Select output folder",
      description = "Choose where the extracted CSV, shapefiles, and optional plots will be saved. Then click Submit to start the extraction."
    )$
      step(
      el = ns("guide_output_options"),
      title = "Output options",
      description = paste(
        "Use these switches to control optional outputs.",
        "Save Plots exports individual plot images.",
        "Save Masked Plots exports soil-masked plot images.",
        "Save Time Series creates per-plot time-series figures."
      )
    )$
      step(
      el = ns("guide_segmentation_settings"),
      title = "Segmentation settings",
      description = paste(
        "Segmentation Index: selects the index used to separate vegetation from soil.",
        "",
        "Remove above?: controls whether values above the threshold are masked.",
        "",
        "Threshold: sets the cutoff value.",
        "",
        "VI without mask: calculates one index from the original image without masking.",
        sep = "<br>"
      )
    )
    # Tour
    session$onFlushed(function() {
      guide$init()
    }, once = TRUE)
    observeEvent(input$start_autoextract_tour, {
      guide$start()
    })

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

    # Plot Shape: original uploaded file
    plot_shape_raw <- reactive({
      req(input$plot_shape)
      st_read(dsn = input$plot_shape$datapath, quiet = TRUE)
    })

    # Plot Shape: filtered version used downstream
    plot_shape <- reactive({
      req(plot_shape_raw())
      shp <- plot_shape_raw()
      selected_levels <- input$subset_shape_level
      if (
        !is.null(input$color_by) &&
          !is.null(selected_levels) &&
          !"All" %in% selected_levels &&
          length(selected_levels) > 0
      ) {
        shp <- shp |>
          dplyr::filter(
            as.character(.data[[input$color_by]]) %in% selected_levels
          )
      }
      shp
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
        rgb_dir <- parseDirPath(roots, input$directory_rgb)
        req(length(rgb_dir) == 1)
        req(dir.exists(rgb_dir))
        path_rgb(rgb_dir)
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

    observeEvent(path_rgb(),
      {
        req(path_rgb())
        rgb_dir <- path_rgb()
        validate(
          need(length(rgb_dir) == 1, "RGB directory was not selected correctly."),
          need(dir.exists(rgb_dir), "Selected RGB directory does not exist.")
        )
        imgs <- list.files(path = rgb_dir, pattern = "\\.tif$", full.names = TRUE)
        validate(
          need(length(imgs) > 0, "No .tif files were found in the selected RGB directory.")
        )
        total_imgs <- length(imgs)
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "subset_img",
          choices = seq_len(total_imgs),
          selected = seq_len(total_imgs)
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
    observeEvent(plot_shape_raw(), {
      updateSelectInput(
        session = session,
        inputId = "plot_id",
        choices = colnames(plot_shape_raw())
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
        if (input$angle == "auto" | is.null(input$angle)) {
          angle <- get_plot_angle(plot_shape()[1, ])
          angle <- angle - 90
        } else {
          angle <- as.numeric(input$angle)
        }
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
                      index_no_mask = index_no_mask,
                      angle_grid = angle
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
      validate(
        need(nrow(plot_shape()) > 0, "No plots match the selected level.")
      )
      map <- mapview(
        x = plot_shape(),
        alpha.regions = 0.5,
        aplha = 1,
        zcol = input$color_by
      )
      map@map
    })

    # Update filter plot_shape
    observeEvent(input$color_by, {
      req(plot_shape_raw(), input$color_by)
      vals <- plot_shape_raw() |>
        sf::st_drop_geometry() |>
        dplyr::pull(input$color_by)
      vals <- vals |>
        as.character() |>
        unique() |>
        na.omit() |>
        sort()
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "subset_shape_level",
        choices = c("All", vals),
        selected = "All"
      )
    })

    # Modal
    observeEvent(input$view_shape, {
      req(plot_shape())
      showModal(modalDialog(
        title = tagList(icon = icon("table-cells"), "View Shape"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        fluidRow(
          column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("color_by"),
              label = "Select Column:",
              choices = colnames(plot_shape()),
              options = shinyWidgets::pickerOptions(
                container = "body",
                style = "btn-outline-secondary"
              )
            )
          ),
          column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("subset_shape_level"),
              label = "Subset selected level:",
              choices = "All",
              selected = "All",
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                container = "body",
                style = "btn-outline-secondary",
                actionsBox = TRUE
              )
            )
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
