#' 00_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_00_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 2),
      column(
        width = 8,
        br(),
        h2("Welcome to the HTP Data Analysis App!"),
        hr(),
        p("This Shiny app is designed to assist researchers and plant breeders working with
        High-Throughput Phenotyping (HTP) data. It offers user-friendly tools to extract
        plot-level information from RGB and Multispectral images. The app simplifies
        data handling for plant breeding experiments."),
        h4("Explore the following modules:"),
        tags$ol(
          tags$li(
            actionLink(inputId = ns("link_1"), label = strong("exploreHTP:")),
            "extract plot-level information by providing a grid shape file and the image paths."
          ),
          tags$li(
            actionLink(inputId = ns("link_2"), label = strong("Visualizer:")),
            "visualize plot time series from cropped images generated in exploreHTP."
          ),
          tags$li(
            actionLink(inputId = ns("link_3"), label = strong("Resizer:")),
            "Grid resizing tool to easily modify existing grid files."
          ),
          tags$li(
            actionLink(inputId = ns("link_4"), label = strong("flexFitR:")),
            "Non-linear regression models using flexFitR for model fitting."
          )
        ),
        hr(),
        h5("Start using the app by navigating to the modules from the navbar menu."),
        tags$div(
          style = "margin-top: 20px;",
          tags$img(
            src = "www/RStudio_logo_flat.svg",
            alt = "RStudio Logo",
            width = "100"
          )
        )
      ),
      column(width = 2)
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
