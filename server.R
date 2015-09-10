
# util --------------------------------------------------------------------

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# data --------------------------------------------------------------------

# read data
dt <- fread("data.csv", encoding = "UTF-8")
dt_col <- fread("colname_cn.csv", encoding = "UTF-8") %>% setkey(EN)

# server ------------------------------------------------------------------

function(input, output, session) {
  if_en <- reactive(input$query_lang == "English")
  observe({
    updateSelectizeInput(
      session, "query_name",
      choices = if (!if_en()) {
        dt[, Name]
      } else {
        dt[, Name_EN]
      }
    )
  })
  query_dt <- reactive({
    input$query_name
    isolate({
      if (if_en()) 
        r <- dt %>% dplyr::filter(Name_EN == input$query_name)
      else
        r <- dt %>% dplyr::filter(Name == input$query_name)
      r[, popup := paste0(p(Name), p(na2blank(Name_EN)), p(na2blank(Address)), collapse = "")]
    })
    r
  })
  output$location <- renderLeaflet({
    leaflet(query_dt()) %>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(dt)) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~LNG, lat = ~LAT
      ) %>%
      addPopups(
        lng = ~LNG, lat = ~LAT, popup = ~popup
      ) %>% 
      setView(lng = query_dt()$LNG, lat = query_dt()$LAT, zoom = 5)
  })
  output$global_map <- renderLeaflet({
    leaflet(dt) %>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(dt)) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~LNG, lat = ~LAT, popup = ~Name
      )
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
  # info maintenance
  # if add then clear info; if modify then load info
  observe({
    if (input$info_mode == "add") {
      isolate({
        shinyjs::reset("info_class")
        shinyjs::reset("info_class_en")
        shinyjs::reset("info_name")
        shinyjs::reset("info_name_en")
        shinyjs::reset("info_area")
        shinyjs::reset("info_country")
        shinyjs::reset("info_lng")
        shinyjs::reset("info_lat")
        session$sendInputMessage("info_address", list(value = ""))
        session$sendInputMessage("info_website", list(value = ""))
      })
    }
  })
  observe(
    if (input$info_mode == "modify") {
      dt_tgt <- data() %>% dplyr::filter(Name == input$info_target)
      isolate({
        updateSelectizeInput(session, "info_class", selected = dt_tgt[, Class])
        updateSelectizeInput(session, "info_class_en", selected = dt_tgt[, Class_EN])
        updateTextInput(session, "info_name", value = dt_tgt[, Name])
        updateTextInput(session, "info_name_en", value = dt_tgt[, Name_EN])
        updateSelectizeInput(session, "info_area", selected = dt_tgt[, Area])
        updateSelectizeInput(session, "info_country", selected = dt_tgt[, Country])
        updateNumericInput(session, "info_lng", value = dt_tgt[, LNG])
        updateNumericInput(session, "info_lat", value = dt_tgt[, LAT])
        session$sendInputMessage("info_address", list(value = dt_tgt[, Address]))
        session$sendInputMessage("info_website", list(value = dt_tgt[, Website]))
      })
    }
  )
  # submit info maintenance
  observeEvent(
    input$info_submit,
    switch(
      input$info_mode,
      "add" = {
        if (is.null(input$info_lng)) {
          shinyjs::info("LNG needs a number!")
          return()
        }
        if (is.null(input$info_lat)) {
          shinyjs::info("LAT needs a number!")
          return()
        }
        
      }
    )
  )
}
