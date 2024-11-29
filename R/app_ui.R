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
    # page_fillable(
    #   titlePanel("Rotate Simple Feature"),
    #   class = "bslib-page-dashboard",
    #   # use default Bootstrap styles
    #   theme = bs_theme(preset = "bootstrap"),
    #   # input_dark_mode(id = "col"),
    #   mod_01_grid_resize_ui("01_grid_resize_1")
    # )
    page_navbar(
      title = "Image Analizer",
      theme = bs_theme(
        preset = "shiny",
        version = 5
      ),
      collapsible = TRUE,
      id = "navbar",
      fillable = TRUE,
      nav_panel(
        title = "Resizer",
        layout_sidebar(
          sidebar = sidebar(
            helpText(
              "Make sure about the format of the shapefile, it needs to be
              .gpkg, also notice that the Mosaic file is optional, and if this
              file is too big it can slow things down."),
            open = "closed",
            bg = "white",
            title = "Help"
          ),
          mod_01_grid_resize_ui("01_grid_resize_1")
        )
      ),
      nav_panel(
        title = "Autoextract",
        # mod_01_grid_resize_ui("01_grid_resize_1")
      ),
      nav_panel(
        title = "flexFitR",
        # mod_01_grid_resize_ui("01_grid_resize_1")
      ),
      nav_panel(
        title = "About",
        # mod_01_grid_resize_ui("01_grid_resize_1")
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
