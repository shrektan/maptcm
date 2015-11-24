
# server ------------------------------------------------------------------

function(input, output, session) {
  
  # data table
  output$data <- renderDataTable({
    r <- data()
    DT::datatable(r, 
                  escape = c(-2), 
                  class = "hover row-border nowrap stripe",
                  selection = "none")
  }, server = FALSE)
  
  # main server
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/" target = "_blank">Mapbox</a>'
      ) %>% 
      setView(lng = 0, lat = 30, zoom = 2)
  })
  
  def_icons <- iconList(
    College = makeIcon("icons/university.svg", "icons/university.svg", 12 * 2.5, 12 * 2.5),
    Hospital = makeIcon("icons/hospital.svg", "icons/hospital.svg", 12 * 2.5, 12 * 2.5),
    Association = makeIcon("icons/Association.svg", "icons/Association.svg", 12 * 2.5, 12 * 2.5),
    Society = makeIcon("icons/Society.svg", "icons/Society.svg", 12 * 2.5, 12 * 2.5)
  )
  
  observe({
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addMarkers(~Lon, ~Lat, layerId = ~Name, icon = ~def_icons[Class], 
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
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
    # detail info
    output$detailed_info <- renderUI({
      isolate({
        tmp <- data() %>% dplyr::filter(Name == name) %>% 
          dplyr::select(-(Lat:TimeStamp), -GoTo)
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
    # show
    shinyjs::show(id = "controls", anim = TRUE, animType = "slide", time = 1.0)
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
      show_popup(name, lat, lng)
      map %>% setView(lng = 0, lat = 30, zoom = 2)
      Sys.sleep(0.01)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  observeEvent(
    input$back, {
      shinyjs::hide(id = "controls", anim = TRUE, animType = "slide", time = 1)
      leafletProxy("map") %>% 
        clearPopups() %>%
        setView(lng = 0, lat = 30, zoom = 2)
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
