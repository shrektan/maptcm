
# global setting ----------------------------------------------------------

options(stringsAsFactors = FALSE)
library(magrittr)
library(data.table)
library(shiny)
library(shinythemes)
library(DT)
library(shinyjs)
library(shinyBS)
library(leaflet)

# ui ----------------------------------------------------------------------

navbarPage(
  title = "China Medicine University Distribution",
  theme = shinytheme("united"),
  collapsible = TRUE,
  tabPanel(
    title = "Global Map",
    leafletOutput("global_map", height = "800px")
  ),
  tabPanel(
    title = "Query",
    bsCollapse(
      multiple = TRUE,
      open = c("Query Table", "Location"),
      bsCollapsePanel(
        title = "Query Table",
        dataTableOutput("query_table"),
        tags$style(type = "text/css", "#query_table{font-size:12px}")
      ),
      bsCollapsePanel(
        title = "Location",
        leafletOutput("location")
      )
    )
  )
)
