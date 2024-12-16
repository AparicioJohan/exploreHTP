#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  selector <- mod_00_home_server("00_home_1")

  observeEvent(selector$link_1(), {
    nav_select(id = "main_navbar", session, selected = "Autoextract")
  })
  observeEvent(selector$link_2(), {
    nav_select(id = "main_navbar", session, selected = "Visualizer")
  })
  observeEvent(selector$link_3(), {
    nav_select(id = "main_navbar", session, selected = "Resizer")
  })
  observeEvent(selector$link_4(), {
    nav_select(id = "main_navbar", session, selected = "Modeling")
  })

  mod_01_grid_resize_server("01_grid_resize_1")
  mod_02_auto_extract_server("02_auto_extract_1")
  mod_03_plot_visual_server("03_plot_visual_1", dark_mode = reactive(input$dark_mode))
  mod_04_flexfitr_server("04_flexfitr_1", dark_mode = reactive(input$dark_mode))
}
