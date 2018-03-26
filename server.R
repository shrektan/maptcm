abbr_text <- function(x, length, suffix = "...") {
  if (length(x) > 1) return(purrr::map_chr(x, abbr_text, length = length, suffix = suffix))
  stopifnot(assertthat::is.string(x), assertthat::is.number(length), length >= 1L)
  if (isTRUE(is.na(x))) return("")
  flag_abbr <- (length < stringr::str_length(x))
  if (isTRUE(flag_abbr)) {
    res <- tags$abbr(paste0(stringr::str_sub(x, 1, length), suffix), title = x, content = x)
  } else {
    res <- htmltools::htmlEscape(x)
  }
  res <- stringr::str_replace_all(res, "\n", "/")
  as.character(res)
}

# server ------------------------------------------------------------------

function(input, output, session) {
  
  # data table
  output$data <- DT::renderDataTable({
    r <- 
      data() %>% 
      dplyr::select(-Lon, -Lat, -ifDeleted, -TimeStamp) %>%
      dplyr::mutate(
        NameCN = abbr_text(NameCN, length = 18L),
        Name = abbr_text(Name, length = 48L),
        Class = abbr_text(Class, length = 15),
        Country = abbr_text(Country, length = 15),
        City = abbr_text(City, length = 15),
        Website = purrr::map_chr(Website, ~as.character(tags$a(., href = sprintf("http://%s", .)))),
        Address = abbr_text(Address, length = 30)
      )
    DT::datatable(
      r, 
      escape = FALSE, 
      class = "nowrap hover row-border stripe",
      selection = "single",
      colnames = c(" ", "中文名称 CN", "英文名称 EN", 
                   "类别 CLASS", "国家 COUNTRY", "城市 CITY", 
                   "具体地址 ADDRESS", "网址 WEBSITE"), 
      options = list(
        scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.16/i18n/Chinese.json"),
        columnDefs = list(list(orderable = FALSE, targets = 0:1))
      )
    )
  }, server = TRUE)
  
  # main server
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}@2x.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/" target = "_blank">Mapbox</a>'
      ) %>% 
      setView(lng = 180 - lonDrift, lat = 30, zoom = 2) %>%
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
