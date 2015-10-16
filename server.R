
# server ------------------------------------------------------------------

function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "info_target",
                         choices = data()[, Name])
    updateSelectizeInput(session, "info_class",
                         choices = data()[, Class])
    updateSelectizeInput(session, "info_class_en",
                         choices = data()[, ClassEN])
    updateSelectizeInput(session, "info_area",
                         choices = data()[, Area])
    updateSelectizeInput(session, "info_country",
                         choices = data()[, Country])
  })
  
  # data table
  output$data <- renderDataTable({
    r <- data()
    DT::datatable(r, options = list(scrollX = TRUE), 
                  escape = c(-2), 
                  class = "hover row-border nowrap stripe",
                  selection = "none")
  }, server = TRUE)
  
  # main server
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      setView(lng = 0, lat = 30, zoom = 2)
  })
  
  def_icons <- iconList(
    College = makeIcon("icons/university.svg", "icons/university.svg", 12 * 1.5, 12 * 1.5),
    Hospital = makeIcon("icons/hospital.svg", "icons/hospital.svg", 12 * 1.5, 12 * 1.5),
    Association = makeIcon("icons/Association.svg", "icons/Association.svg", 12 * 1.5, 12 * 1.5),
    Society = makeIcon("icons/Society.svg", "icons/Society.svg", 12 * 1.5, 12 * 1.5)
  )
  
  observe({
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addMarkers(~LNG, ~LAT, layerId = ~Name, icon = ~def_icons[ClassEN])
  })
  
  output$detailed_info <- renderUI({
    event <- input$map_marker_click
    if (is.null(event)) return()
    isolate({
      tmp <- data() %>% dplyr::filter(Name == event$id) %>% 
        dplyr::select(-(LAT:TimeStamp), -GoTo)
      tmp <- tmp[, which(!is.na(as.character(tmp))), with = FALSE]
      tmp_c <- dt_col[J(colnames(tmp)), CNEN]
      r <- vector("list", length(tmp_c))
      for (i in 1:ncol(tmp)) {
        r[[i]] <- p(tags$span(tmp_c[i], ":"),
                    na2blank(as.character(tmp[, i, with = FALSE])))
      }
      tagList(r)
    })
  })
 
  # Show a popup at the given location
  show_popup <- function(name, lat, lng) {
    r <- data() %>% dplyr::filter(Name == name)
    content <- as.character(tagList(
     p(tags$span(dt_col[J("Name"), CNEN], ": ", style = "font-weight:bold;"),
       br(), br(), r$Name, br(), r$NameEN),
     p(tags$span(dt_col[J("Address"), CNEN], ": ", style = "font-weight:bold;"), 
       r$Address),
     p(tags$span(dt_col[J("Website"), CNEN], ": ", style = "font-weight:bold;"),
       a(r$Website, href = r$Website, target = "_blank"))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = name)
    shinyjs::show("controls")
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event)) return()
    isolate({
      show_popup(event$id, event$lat, event$lng)
    })
  })
  
  observe({
    if (is.null(input$goto)) return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.1
      name <- input$goto$name
      lat <- input$goto$lat
      lng <- input$goto$lng
      show_popup(name, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  source("server_info.R", local = TRUE)
  
  output$download <- downloadHandler(
    filename = function() sprintf("map-data-%s.xlsx", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      openxlsx::write.xlsx(dt, file, as.Table = FALSE)
    }
  )
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 1.5)    
}
