
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
      selectizeInput("query_name", label = NULL, choices = NULL),
      tags$style(type = "text/css", "#query_name{font-size:11px};"),
      br()
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
          "#detailed_info{font-size:12px}")
      )
    ),
    tabItem(
      "Info",
      fluidRow(
        column(
          width = 6,
          radioButtons(
            "info_mode", NULL, 
            c("增加/Add" = "add", "修改/Modify" = "modify", "删除/Delete" = "delete"), 
            inline = TRUE)
        ),
        column(
          width = 6,
          conditionalPanel(
            "input.info_mode != 'add'",
            selectizeInput(
              "info_target",
              NULL,
              choices = dt[, Name]
            )
          )
        )
      ),
      conditionalPanel(
        "input.info_mode != 'delete'",
        bsCollapse(
          open = "Info Panel",
          bsCollapsePanel(
            "Info Panel",
            fluidRow(
              column(
                width = 6,
                selectizeInput("info_class", "类别/Class", 
                               choices = dt[, unique(Class)],
                               options = list(create = TRUE))
              ),
              column(
                width = 6,
                selectizeInput("info_class_en", "英文类别/Class_EN", 
                               choices = dt[, unique(Class_EN)],
                               options = list(create = TRUE))
              )
            ),
            fluidRow(
              column(
                width = 6,
                textInput("info_name", "名称/Name")
              ),
              column(
                width = 6,
                textInput("info_name_en", "英文名称/Name_EN")
              )
            ),
            fluidRow(
              column(
                width = 3,
                selectizeInput("info_area", "区域/Area", 
                               choices = dt[, unique(Area)],
                               options = list(create = TRUE))
              ),
              column(
                width = 3,
                selectizeInput("info_country", "国家/Country", 
                               choices = dt[, unique(Country)],
                               options = list(create = TRUE))
              ),
              column(
                width = 3,
                numericInput("info_lng", "经度/LNG", 0, -180, 180, 0.01)
              ),
              column(
                width = 3,
                numericInput("info_lat", "纬度/LAT", 0, -180, 180, 0.01)
              )
            ),
            fluidRow(
              column(
                width = 12,
                tags$label(
                  class = "control-label", `for` = "info_area",
                  "地址/Address"
                ),
                tags$style(type = "text/css", "textarea {width:100%}"),
                tags$textarea(id = 'info_address',
                              placeholder = 'Type your address here', rows = 4, width = "100%")
              )
            ),
            fluidRow(
              column(
                width = 12,
                tags$label(
                  class = "control-label", `for` = "info_website",
                  "网址/Website"
                ),
                tags$style(type = "text/css", "textarea {width:100%}"),
                tags$textarea(id = 'info_website',
                              placeholder = 'Type your website here', rows = 2, width = "100%")
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          actionButton("info_submit", "提交/Submit")
        )
      )
    )
  )
)

# ui ----------------------------------------------------------------------

dashboardPage(
  title = "Chinese Medicine Universities",
  skin = "yellow",
  header = header,
  sidebar = sidebar,
  body = body
)



