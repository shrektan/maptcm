

# new ui ------------------------------------------------------------------


navbarPage(
  title = "Map of CTM",
  id = "nav",
  tabPanel(
    "Map",
    icon = icon("globe"),
    div(
      class = "outer",
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        shinyjs::useShinyjs(),
        inlineCSS(loading_css)),
      # Loading message
      div(
        id = "loading-content",
        br(),
        br(),
        br(),
        br(),
        h2("Loading...")
      ),
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Detailed Info"),
        uiOutput("detailed_info")
      ) %>% shinyjs::hidden(),
      tags$div(
        id = "cite",
        'Data provided by ',
        tags$em('Yang, Zheng')
      ),
      HTML(
        'Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a>             is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0">CC BY 3.0</a>'
      )
    )
  ),
  
  tabPanel(
    "Data explorer",
    icon = icon("search"),
    hr(),
    dataTableOutput("data")
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
    .,
    tags$a(
      href = "https://github.com/shrektan/CMUnivMap",
      target = "_blank",
      tags$img(
        style = "position: absolute; top: 0; right: 0; border: 0;z-index : 9999",
        src = "https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67",
        alt = "Fork me on GitHub",
        `data-canonical-src` = "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"
      )
    )
  )
