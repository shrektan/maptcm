
# global setting ----------------------------------------------------------

# options(shiny.autoreload = TRUE)
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

# Lon drift ---------------------------------------------------------------

lonDrift <- 30L

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

dt_col <- 
  fread("data/colname_cn.csv", encoding = "UTF-8") %>% setkey(EN)
dt_col[, CNEN := paste0(CN, "/", EN)]

load_data <- function(path) {
  tmp <- 
    read_csv(path, 
             col_types = cols(ifDeleted = col_logical(), 
                              TimeStamp = col_datetime())) %>%
    dplyr::mutate(Lon = ifelse(Lon < -lonDrift, Lon + 360, Lon)) %>%
    setDT()
  f_ <- function(lat, lng, name) {
    stopifnot(length(lat) == 1, length(lng) == 1, length(name) == 1)
    as.character(tags$a(class = "go-map", href = "", 
                        `data-lat` = lat,
                        `data-long` = lng,
                        `data-name` = name,
                        tags$i(class = "fa fa-crosshairs")))
  }
  dt <- 
    tmp[, MaxTimeStamp := max(TimeStamp), by = Name][
      MaxTimeStamp == TimeStamp
      ][, MaxTimeStamp := NULL][ifDeleted == FALSE]
  dt[, GoTo := Map(f_, Lat, Lon, Name) %>% as.character()]
  tmp <- colnames(dt)[colnames(dt) != "GoTo"]
  dt[, c("GoTo", tmp), with = FALSE][]
}

data <- reactiveFileReader(
  1000, NULL, "data/data.csv", load_data
)

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

# update data
update_data <- function(r) {
  r <- rbindlist(list(data()[FALSE], r), fill = TRUE)
  # backup
  backup_data()
  # write csv
  cns <- readr::read_csv("data/data.csv", n_max = 0) %>% colnames()
  write_csv(r[, cns, with = FALSE], 
            "data/data.csv", 
            append = file.exists("data/data.csv"))
  # return
  invisible()
}
