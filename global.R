
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
library(readr)

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

# define error message
shinyjs_validate <- function(...) {
  ls <- list(...)
  tmp <- vapply(ls, is.null, FUN.VALUE = TRUE)
  ls <- ls[which(!tmp)]
  if (length(ls) == 0) return(invisible())
  shinyjs::info(paste0(ls, collapse = " "))
  cond <- structure(
    list(message = ""), 
    class = c("validation", 
              "shiny.silent.error", "error", "condition"))
  stop(cond)
}

# bs Panel
bsPanel <- function(theme = "default", header, style = NULL, ...) {
  div(
    class = paste0("panel panel-", theme),
    style = style,
    div(class = "panel-heading", header),
    div(class = "panel-body", ...)
  )
}

# data --------------------------------------------------------------------

dt <- dt_col <- NULL

# load data
load_data <- function() {
  tmp <- 
    read_csv("data/data.csv", 
             col_types = cols(ifDeleted = col_logical(), 
                              TimeStamp = col_datetime())) %>%
    setDT()
  dt <<- 
    tmp[, MaxTimeStamp := max(TimeStamp), by = Name][
      MaxTimeStamp == TimeStamp
    ][, MaxTimeStamp := NULL][ifDeleted == FALSE]
  dt_col <<- 
    fread("data/colname_cn.csv", encoding = "UTF-8") %>% setkey(EN)
  dt_col[, CNEN := paste0(CN, "/", EN)]
  invisible()
}

load_data()

# define backup data
backup_data <- function() {
  if (!dir.exists("./csv_his")) dir.create("./csv_his")
  backup_file <- 
    file.path(
      "./csv_his", 
      paste0(format(Sys.time(), "%Y%m%d%H%M%S"),
             basename(tempfile(pattern = "")), ".csv")
    )
  file.copy("./data/data.csv", backup_file)
  invisible()
}
# establish server push
server_push <- 0L

# update data
update_data <- function(r) {
  # backup
  backup_data()
  # write csv
  write_csv(r[, colnames(dt), with = FALSE], 
            "data/data.csv", 
            append = file.exists("data/data.csv"))
  # read csv
  load_data()
  # server push
  server_push <<- server_push + 1L
  # return
  invisible()
}
