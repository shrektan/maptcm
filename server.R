
# Lon drift ---------------------------------------------------------------

lonDrift <- 90L

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
# server ------------------------------------------------------------------

function(input, output, session) {
  
  # data table
  output$data <- DT::renderDataTable({
    r <- data() %>% dplyr::select(-Lon, -Lat, -ifDeleted, -TimeStamp)
    DT::datatable(r, 
                  escape = c(-2), 
                  class = "nowrap hover row-border stripe",
                  selection = "none",
                  options = list(scrollX = TRUE))
  }, server = TRUE)
  
  # main server
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        "http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
        # urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}@2x.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/" target = "_blank">Mapbox</a>',
        tileOptions(tileSize = 256, minZoom = 3, maxZoom = 17)
      ) %>% 
      setView(lng = 180 - lonDrift, lat = 30, zoom = 3) %>%
      setMaxBounds(-lonDrift, -75, -lonDrift + 360, 90)
  })
  
  def_icons <- iconList(
    College = makeIcon("icons/university.svg", "icons/university.svg", 12 * 2.5, 12 * 2.5),
    Hospital = makeIcon("icons/hospital.svg", "icons/hospital.svg", 12 * 2.5, 12 * 2.5),
    Association = makeIcon("icons/Association.svg", "icons/Association.svg", 12 * 2.5, 12 * 2.5),
    Society = makeIcon("icons/Society.svg", "icons/Society.svg", 12 * 2.5, 12 * 2.5)
  )
  
  observe({
    pal <- colorFactor("Set1", domain = sort(unique(data()$Class)))
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircleMarkers(
        ~Lon, ~Lat, layerId = ~Name,
        radius = 6L,
        color = ~pal(Class),
        stroke = FALSE,
        fillOpacity = 0.8
      )
      # addMarkers(~Lon, ~Lat, layerId = ~Name) #, #icon = ~def_icons[Class], 
                 # clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                       # spiderfyOnMaxZoom = FALSE))
  })
  
 
  # Show a popup at the given location
  show_popup <- function(name, lat, lng) {
    r <- data() %>% dplyr::filter(Name == name)
    content <- as.character(tagList(
     p(tags$span(dt_col[J("Name"), CNEN], ": "),
       br(), r$NameCN, "/", r$Name),
     p(tags$span(dt_col[J("Class"), CNEN], ": "), 
       r$Class),
     p(tags$span(dt_col[J("Country"), CNEN], ": "), 
       r$Country),
     p(tags$span(dt_col[J("City"), CNEN], ": "), 
       r$City),
     p(tags$span(dt_col[J("Address"), CNEN], ": "), 
       br(), r$Address),
     p(tags$span(dt_col[J("Website"), CNEN], ": "),
       br(),
       a(r$Website, href = paste0("http://", r$Website), target = "_blank"))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = name)
  }
  
  # When map is clicked, show a popup with city info
  observeEvent(
    input$map_marker_click, {
      leafletProxy("map") %>% clearPopups()
      event <- input$map_marker_click
      show_popup(event$id, event$lat, event$lng)
    }
  )
  
  observe({
    if (is.null(input$goto)) return()
    isolate({
      updateNavbarPage(session, "nav", "Map")
      # Sys.sleep(0.01)
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.3
      name <- input$goto$name
      lat <- input$goto$lat
      lng <- input$goto$lng
      # map %>% setView(lng = 180 - lonDrift, lat = 30, zoom = 2)
      # Sys.sleep(0.01)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      show_popup(name, lat, lng)
    })
  })
  
  observeEvent(
    input$back, {
      shinyjs::hide(id = "controls", anim = TRUE, animType = "slide", time = 1)
      leafletProxy("map") %>% 
        clearPopups() %>%
        setView(lng = 180 - lonDrift, lat = 30, zoom = 2)
    }
  )
  
  # source("server_info.R", local = TRUE)
  
  output$download <- downloadHandler(
    filename = function() sprintf("map-data-%s.xlsx", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      openxlsx::write.xlsx(dt, file, as.Table = FALSE)
    }
  )
}
