
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

# loading css -------------------------------------------------------------

loading_css <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}"

# util --------------------------------------------------------------------

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# data --------------------------------------------------------------------

# read data
dt <- fread("data.csv", encoding = "UTF-8")
dt_col <- fread("colname_cn.csv", encoding = "UTF-8") %>% setkey(EN)

# establish server push
server_push <- 0L
