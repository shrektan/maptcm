

# new ui ------------------------------------------------------------------
navbarPage(
  title = "Map of TCM",
  id = "nav",
  # inverse = TRUE,
  theme = shinytheme("cosmo"),
  tabPanel(
    "Map",
    icon = icon("globe"),
    div(
      class = "outer",
      # Loading message
      div(
        id = "loading-content",
        br(),
        br(),
        br(),
        br(),
        h2("Loading...")
      ) %>% shinyjs::hidden(),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 10, bottom = "auto",
        width = 350, height = "auto",
        absolutePanel(
          top = 8, left = 20, actionLink("back", NULL, icon = icon("globe"))
        ),
        hr(),
        uiOutput("detailed_info"),
        hr()
      ) %>% shinyjs::hidden(),
      tags$div(
        id = "cite",
        p(
          'Data provided by ',
          tags$em('Yang, Zheng.'),
          "Powered by ",
          tags$a(href = "http://shiny.rstudio.com", target = "_blank", "Shiny"),
          "."
        ),
        p(
          HTML(
            'Icons made by <a href="http://www.freepik.com" 
            title="Freepik">Freepik</a> from 
            <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> 
            is licensed by <a href="http://creativecommons.org/licenses/by/3.0/"
            title="Creative Commons BY 3.0">CC BY 3.0</a>'
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Data explorer",
    icon = icon("search"),
    hr(),
    fluidRow(
      column(
        width = 12, 
        div(
          class = "dt",
          dataTableOutput("data")
        )
      )
    )
  ),
  tabPanel(
    "Info Maintain",
    icon = icon("info"),
    source("ui_info.R", local = TRUE)$value
  ),
  tabPanel(
    "Download Data",
    icon = icon("cloud-download"),
    p("Click here to ", downloadLink("download", "download data"), ".")
  )
) %>% 
  tagList(
    # Include our custom CSS
    tags$head(
      includeCSS("styles.css"),
      includeScript("gomap.js"),
      shinyjs::useShinyjs()
    ),
    .,
    tags$a(
      href = "https://github.com/shrektan/CMUnivMap",
      target = "_blank",
      tags$img(
        style = "position: absolute; top: 0; right: 0; border: 0; z-index: 5000",
        src = "github-label-grey.png",
        alt = "Fork me on GitHub",
        `data-canonical-src` = 
          "https://s3.amazonaws.com/github/ribbons/forkme_right_white_ffffff.png"
      )
    )
  )
