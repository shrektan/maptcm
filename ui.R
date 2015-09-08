
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
      tabName = "GlobalMap"
    ),
    menuItem(
      "Query",
      tabName = "Query"
    ),
    menuItem(
      "Info Maintenance",
      tabName = "Info"
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
        title = "Query",
        width = 6,
        solidHeader = TRUE,
        dataTableOutput("query_table"),
        tags$style(type = "text/css", "#query_table{font-size:11px}")
      ),
      box(
        title = "Location",
        width = 6,
        solidHeader = TRUE,
        leafletOutput("location")
      )
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



