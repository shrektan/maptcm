

# new ui ------------------------------------------------------------------
navbarPage(
  title = "Map of TCM",
  id = "nav",
  inverse = TRUE,
  theme = shinytheme("spacelab"),
  collapsible = TRUE,
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
          'Contributed by ',
          tags$em('Tsinghua university, Xiaomei Xu, Zheng Yang, Lai Yu.')
        ),
        p(
          "App made by ", 
          tags$a(href = "mailto:shrektan@126.com", "Xianying Tan"),
          ".",
          "Powered by ",
          tags$a(href = "http://shiny.rstudio.com", target = "_blank", "Shiny"),
          "."
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
        dataTableOutput("data")
      )
    )
  ),
  tabPanel(
    "Info & Disqus",
    icon = icon("info"),
    br(),
    p("If you find the info is wrong or you want to add new info, ",
      "please contact ", tags$a(href = "mailto:maptcm@126.com", "maptcm@126.com", ".")),
    br(),
    includeHTML("disqus_thread.html")
  )
  # tabPanel(
    # "Download Data",
    # icon = icon("cloud-download"),
    # p("Click here to ", downloadLink("download", "download data"), ".")
  # )
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
      id = "github_ribbon",
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
