
# server ------------------------------------------------------------------

function(input, output, session) {
  # establish dt monitor
  flag_push <- reactivePoll(
    1000, session, checkFunc = function() {
      server_push
    }, valueFunc = function() {
      server_push
    }
  )
  data <- reactive({
    flag_push()
    dt
  })
  observe({
    updateSelectizeInput(session, "info_target",
                         choices = data()[, Name])
    updateSelectizeInput(session, "info_class",
                         choices = data()[, Class])
    updateSelectizeInput(session, "info_class_en",
                         choices = data()[, Class_EN])
    updateSelectizeInput(session, "info_area",
                         choices = data()[, Area])
    updateSelectizeInput(session, "info_country",
                         choices = data()[, Country])
  })
  
  # main server
  if_en <- reactive(FALSE)#input$query_lang == "English")
  observe({
    updateSelectizeInput(
      session, "query_name",
      choices = if (!if_en()) {
        data()[, Name]
      } else {
        data()[, Name_EN]
      }
    )
  })
  query_dt <- reactive({
    input$query_name
    isolate({
      if (if_en()) 
        r <- data() %>% dplyr::filter(Name_EN == input$query_name)
      else
        r <- data() %>% dplyr::filter(Name == input$query_name)
      r[, popup := paste0(p(Name), p(na2blank(Name_EN)), p(na2blank(Address)), collapse = "")]
    })
    r
  })
  output$location <- renderLeaflet({
    leaflet(query_dt()) %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(data())) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~LNG, lat = ~LAT
      ) %>%
      addPopups(
        lng = ~LNG, lat = ~LAT, popup = ~popup
      ) %>% 
      setView(lng = query_dt()$LNG, lat = query_dt()$LAT, zoom = 13)
  })
  output$map <- renderLeaflet({
    leaflet(data()) %>% 
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/shrektan.ciffhrg2x8fe2suknjq6qv5g7/{z}/{x}/{y}.png?access_token=pk.eyJ1Ijoic2hyZWt0YW4iLCJhIjoiY2lmZmhyaTR3OGczeHNtbHhyb2Rjb2cwcSJ9.c2vjzcma6a24uYuUpyXUWQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(data())) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~LNG, lat = ~LAT, popup = ~Name
      ) %>% 
      setView(lng = 0, lat = 30, zoom = 2)
  })
  output$detailed_info <- renderUI({
    tmp <- copy(query_dt())
    tmp[, c("LNG", "LAT", "popup") := NULL]
    tmp2 <- as.character(tmp)
    tmp <- tmp[, which(!is.na(tmp2)), with = FALSE]
    tmp_c <- colnames(tmp)
    if (!if_en()) tmp_c <- dt_col[J(tmp_c), CN]
    tmp <- paste0(tmp_c, ": ", na2blank(as.character(tmp)))
    HTML(
      paste0(vapply(tmp, function(x) as.character(p(x)), "a"), collapse = "")
    )
  })
 
  source("server_info.R", local = TRUE)
  
  output$download <- downloadHandler(
    filename = function() sprintf("map-data-%s.xlsx", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      openxlsx::write.xlsx(dt, file, as.Table = FALSE)
    }
  )
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
}
