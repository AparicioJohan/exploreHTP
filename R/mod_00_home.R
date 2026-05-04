#' 00_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon tags div p h1 h5 actionLink
#' @importFrom bslib card card_body layout_columns
mod_00_home_ui <- function(id) {
  ns <- NS(id)

  mk_card <- function(link_id, fa_icon, title, desc, icon_class, btn_class) {
    card(
      fill = FALSE,
      class = "module-card border-0 shadow-sm",
      card_body(
        class = "p-3 d-flex flex-row align-items-center gap-3",
        div(class = paste0("icon-badge flex-shrink-0 ", icon_class), icon(fa_icon)),
        div(
          class = "flex-grow-1",
          h6(title, class = "fw-bold mb-1"),
          p(desc, class = "text-muted small mb-0")
        ),
        actionLink(
          inputId = ns(link_id),
          label = icon("arrow-right"),
          class = paste("btn btn-sm fw-semibold flex-shrink-0", btn_class)
        )
      )
    )
  }

  div(
    style = "max-width: 700px; margin: 0 auto;",

    # Hero Banner
    div(
      class = "hero-banner rounded-4 px-4 py-3 mb-3",
      div(
        class = "d-flex align-items-center gap-3",
        # tags$img(src = "www/logo.png", width = "48px", height = "auto"),
        div(
          h4("exploreHTP", class = "fw-bold mb-0"),
          p(
            "High-Throughput Phenotyping data analysis — extract UAV plot metrics, visualize time series, fit growth models.",
            class = "text-muted small mb-0"
          )
        )
      )
    ),

    # Section label
    p("MODULES", class = "small fw-semibold text-muted letter-spacing-wide mb-2"),

    # Module Cards Grid
    layout_columns(
      col_widths = c(12, 12, 12, 12),
      fill = FALSE,
      mk_card(
        "link_1", "images", "Autoextract",
        "Extract plot-level phenotypes from RGB and multispectral UAV images.",
        "icon-green", "btn-outline-success"
      ),
      mk_card(
        "link_2", "chart-line", "Visualizer",
        "Visualize and export time-series phenology plots from cropped image data.",
        "icon-blue", "btn-outline-primary"
      ),
      mk_card(
        "link_4", "brain", "Modeling",
        "Fit non-linear growth curves to phenotypic trajectories using flexFitR.",
        "icon-purple", "btn-outline-purple"
      ),
      mk_card(
        "link_3", "crop", "Resizer",
        "Interactively resize and rotate plot grid shapefiles before extraction.",
        "icon-orange", "btn-outline-warning"
      )
    )
  )
}

#' 00_home Server Functions
#'
#' @noRd
mod_00_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(
      list(
        link_1 = reactive(input$link_1),
        link_2 = reactive(input$link_2),
        link_3 = reactive(input$link_3),
        link_4 = reactive(input$link_4)
      )
    )
  })
}

## To be copied in the UI
# mod_00_home_ui("00_home_1")

## To be copied in the server
# mod_00_home_server("00_home_1")
