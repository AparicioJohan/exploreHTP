#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @import bslib
#' @import leaflet
#' @import terra
#' @import sf
#' @import mapview
#' @import shinyFiles
#' @import stars
#' @import exactextractr
#' @import ggplot2
#' @import waiter
#' @import flexFitR
#' @importFrom plotly plot_ly plotlyOutput renderPlotly
#' @importFrom dplyr mutate select mutate_all rename filter all_of across
#' @importFrom shinyalert shinyalert
#' @importFrom utils write.csv read.csv
#' @importFrom grDevices rgb
#' @importFrom EBImage otsu
#' @importFrom stats coef
#' @importFrom DT renderDT datatable DTOutput
run_app <- function(
    onStart = NULL,
    options = list(launch.browser = TRUE),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...) {
  options(shiny.maxRequestSize = 5000 * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
