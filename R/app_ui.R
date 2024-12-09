#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = tags$span(
        # tags$img(
        #   src = "www/logo.png",
        #   width = "46px",
        #   height = "auto",
        #   class = "me-3",
        #   alt = "Condor-Sky logo"
        # ),
        "Image analyzer"
      ),
      theme = bs_theme(preset = "shiny", version = 5),
      collapsible = TRUE,
      id = "main_navbar",
      fillable = TRUE,
      nav_panel(
        title = "Home",
        icon = icon("home"),
        mod_00_home_ui("00_home_1")
      ),
      nav_panel(
        title = "Autoextract",
        mod_02_auto_extract_ui("02_auto_extract_1")
      ),
      nav_panel(
        title = "Visualizer",
        mod_03_plot_visual_ui("03_plot_visual_1")
      ),
      nav_panel(
        title = "flexFitR",
        mod_04_flexfitr_ui("04_flexfitr_1")
      ),
      nav_panel(
        title = "Resizer",
        layout_sidebar(
          sidebar = sidebar(
            helpText(
              "Make sure about the format of the shapefile, it needs to be
              .gpkg, also notice that the Mosaic file is optional, and if this
              file is too big it can slow things down."
            ),
            open = "closed",
            title = "Resizer"
          ),
          mod_01_grid_resize_ui("01_grid_resize_1")
        )
      ),
      nav_spacer(),
      nav_item(
        tags$a(
          tags$span(icon("github"), "Source code"),
          href = "https://github.com/AparicioJohan/autoextract",
          target = "_blank"
        )
      ),
      nav_panel(
        title = "About",
        icon = icon("question")
        # mod_01_grid_resize_ui("01_grid_resize_1")
      ),
      nav_item(
        input_dark_mode(id = "dark_mode", mode = "light")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "autoextract"
    ),
    shinytoastr::useToastr(),
    waiter::useWaiter()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
