#' 99_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon tags div p h4 h6 strong em
#' @importFrom bslib card card_body
mod_99_about_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "max-width: 700px; margin: 0 auto;",

    # Hero Banner
    div(
      class = "hero-banner rounded-4 px-4 py-3 mb-3",
      div(
        class = "d-flex align-items-center gap-3",
        div(class = "icon-badge icon-blue flex-shrink-0", icon("circle-info")),
        div(
          h4("About exploreHTP", class = "fw-bold mb-0"),
          p("Version 1.0.0 · High-Throughput Phenotyping data analysis", class = "text-muted small mb-0")
        )
      )
    ),

    # About section
    card(
      fill = FALSE,
      class = "border-0 shadow-sm rounded-4 mb-3",
      card_body(
        class = "p-4",
        div(
          class = "d-flex align-items-center gap-2 mb-3",
          div(class = "icon-badge icon-green flex-shrink-0", icon("seedling")),
          h6("What is exploreHTP?", class = "fw-bold mb-0")
        ),
        p(
          "exploreHTP is a Shiny application designed to assist researchers and plant breeders working with ",
          strong("High-Throughput Phenotyping (HTP) data."),
          " It provides tools to extract plot-level information from ",
          strong("RGB and Multispectral UAV imagery,"),
          " simplifying data handling for field trial experiments.",
          class = "text-muted small mb-2"
        ),
        p(
          "The app also supports fitting ",
          strong("non-linear regression models"),
          " via the ",
          tags$a(
            "flexFitR",
            href = "https://apariciojohan.github.io/flexFitR/",
            target = "_blank",
            class = "text-decoration-none fw-semibold"
          ),
          " package, enabling phenotypic trajectory analysis over time.",
          class = "text-muted small mb-0"
        )
      )
    ),

    # Acknowledgements section
    card(
      fill = FALSE,
      class = "border-0 shadow-sm rounded-4 mb-3",
      card_body(
        class = "p-4",
        div(
          class = "d-flex align-items-center gap-2 mb-3",
          div(class = "icon-badge icon-purple flex-shrink-0", icon("star")),
          h6("Acknowledgements", class = "fw-bold mb-0")
        ),
        div(
          class = "d-flex flex-column gap-3",
          div(
            class = "d-flex gap-2 align-items-start",
            icon("user-tie", class = "text-muted mt-1 small flex-shrink-0"),
            p(
              strong("Jeffrey Endelman"),
              " — for his guidance and support throughout the development of this application.",
              class = "text-muted small mb-0"
            )
          ),
          div(
            class = "d-flex gap-2 align-items-start",
            icon("box-open", class = "text-muted mt-1 small flex-shrink-0"),
            p(
              strong("Filipe Matias"),
              " — for his contributions to the ",
              tags$a(
                "FieldImageR",
                href = "https://github.com/OpenDroneMap/FIELDimageR",
                target = "_blank",
                class = "text-decoration-none fw-semibold"
              ),
              " package, which served as inspiration for this app.",
              class = "text-muted small mb-0"
            )
          ),
          div(
            class = "d-flex gap-2 align-items-start",
            icon("graduation-cap", class = "text-muted mt-1 small flex-shrink-0"),
            p(
              "Supported by the ", em("University of Wisconsin-Madison."),
              class = "text-muted small mb-0"
            )
          )
        )
      )
    ),

    # Author section
    card(
      fill = FALSE,
      class = "border-0 shadow-sm rounded-4 mb-3",
      card_body(
        class = "p-4",
        div(
          class = "d-flex align-items-center gap-2 mb-3",
          div(class = "icon-badge icon-orange flex-shrink-0", icon("user")),
          h6("Author", class = "fw-bold mb-0")
        ),
        div(
          h6("Johan Steven Aparicio", class = "fw-bold mb-1"),
          div(
            class = "d-flex gap-2 flex-wrap",
            tags$a(
              icon("github"), " GitHub",
              href = "https://github.com/AparicioJohan",
              target = "_blank",
              class = "btn btn-sm btn-outline-secondary"
            ),
            tags$a(
              icon("envelope"), " Email",
              href = "mailto:johanstevenapa@gmail.com",
              class = "btn btn-sm btn-outline-secondary"
            )
          )
        )
      )
    )
  )
}

#' 99_about Server Functions
#'
#' @noRd
mod_99_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_99_about_ui("99_about_1")

## To be copied in the server
# mod_99_about_server("99_about_1")
