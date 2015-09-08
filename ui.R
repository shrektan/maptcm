
# global setting ----------------------------------------------------------

options(stringsAsFactors = FALSE)
library(magrittr)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinyBS)
library(leaflet)

# ui ----------------------------------------------------------------------

# navbarPage(
#   title = "China Medicine University Distribution",
#   theme = shinytheme("united"),
#   collapsible = TRUE,
#   tabPanel(
#     title = "Global Map",
#     leafletOutput("global_map", height = "800px")
#   ),
#   tabPanel(
#     title = "Query",
#     bsCollapse(
#       multiple = TRUE,
#       open = c("Query Table", "Location"),
#       bsCollapsePanel(
#         title = "Query Table",
#         dataTableOutput("query_table"),
#         tags$style(type = "text/css", "#query_table{font-size:12px}")
#       ),
#       bsCollapsePanel(
#         title = "Location",
#         leafletOutput("location")
#       )
#     )
#   )
# )

# header ------------------------------------------------------------------

header <- dashboardHeader(title = "CMUnivMap")

# sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarvalue",
    menuItem(
      "Global Map",
      tabName = "GlobalMap",
      icon = icon("tags")
    ),
    menuItem(
      "Query",
      tabName = "Query",
      icon = icon("search")
    ),
    conditionalPanel(
      "input.sidebarvalue == 'Query'",
      radioButtons("query_lang", NULL, c("中文", "English"), inline = TRUE),
      dataTableOutput("query_table"),
      tags$style(
        type = "text/css", 
        "#query_table{font-size:11px;background-color:white;color:black}")
    ),
    menuItem(
      "Info Maintenance",
      tabName = "Info",
      icon = icon("info")
    ),
    menuItem(
      "Fork me on GitHub",
      href = "https://github.com/shrektan/CMUnivMap",
      icon = icon("github")
    )
  )
)

# body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(
      "GlobalMap",
      box(
        title = "Global Map",
        width = 12,
        solidHeader = TRUE,
        leafletOutput("global_map", height = "600px")            
      )
    ),
    tabItem(
      "Query",
      box(
        width = 8,
        title = "Location",
        leafletOutput("location", height = "500px")
      ),
      box(
        width = 4,
        title = "Detailed Info",
        uiOutput("detailed_info"),
        tags$style(
          type = "text/css", 
          "#detailed_info{font-size:12px")
      )
    ),
    tabItem(
      "Info",
      "Sorry, it's in process."
    )
  )
)

# ui ----------------------------------------------------------------------

dashboardPage(
  skin = "yellow",
  header = header,
  sidebar = sidebar,
  body = body
)



