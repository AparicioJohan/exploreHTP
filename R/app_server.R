#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_01_grid_resize_server("01_grid_resize_1")
  mod_02_auto_extract_server("02_auto_extract_1")
}
