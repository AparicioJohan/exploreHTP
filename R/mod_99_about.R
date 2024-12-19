#' 99_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_99_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        br(),
        shiny::h3("About This App"),
        shiny::p(
          "This Shiny app is designed to assist researchers and plant breeders working with ",
          shiny::strong("High-Throughput Phenotyping (HTP) data."),
          " It provides user-friendly tools to extract plot-level information from ",
          shiny::strong("RGB and Multispectral images"),
          ", simplifying data handling for plant breeding experiments."
        ),
        shiny::p(
          "In addition to data extraction, the app helps to fit ",
          shiny::strong("Non-linear regression models"),
          " using the ",
          shiny::strong("flexFitR"),
          " package, enabling users to leverage HTP data for model fitting and analysis."
        ),
        shiny::br(),
        shiny::h4("Acknowledgements"),
        shiny::p(
          "I would like to express my gratitude to my professor, ",
          shiny::strong("Jeffrey Endelman"),
          ", for his guidance and support throughout the development of this application."
        ),
        shiny::p(
          "Special thanks to Filipe Matias for his contributions to the ",
          shiny::strong("FieldImageR"),
          " package, which served as inspiration for this app."
        ),
        shiny::p(
          "This project was supported by the ",
          shiny::em("University of Wisconsin-Madison.")
        ),
        shiny::br(),
        shiny::p(
          "Author: ", shiny::strong("Johan Steven Aparicio")
        )
      ),
      column(width = 1)
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
