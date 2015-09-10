
# util --------------------------------------------------------------------

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# data --------------------------------------------------------------------

# read data
dt <- fread("data.csv")

# server ------------------------------------------------------------------

function(input, output, session) {
  observe({
    updateSelectizeInput(
      session, "query_name",
      choices = if (input$query_lang == "中文") {
        dt[, 名称]
      } else {
        dt[, 英文名称]
      }
    )
  })
  query_dt <- reactive({
    input$query_name
    isolate({
      if (input$query_lang == "English") 
        r <- dt %>% dplyr::filter(英文名称 == input$query_name)
      else
        r <- dt %>% dplyr::filter(名称 == input$query_name)
      r[, popup := paste0(p(名称), p(na2blank(英文名称)), p(na2blank(地址)), collapse = "")]
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
        lng = ~lng, lat = ~lat
      ) %>%
      addPopups(
        lng = ~lng, lat = ~lat, popup = ~popup
      ) %>% 
      setView(lng = query_dt()$lng, lat = query_dt()$lat, zoom = 5)
  })
  output$global_map <- renderLeaflet({
    leaflet(dt) %>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(dt)) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~lng, lat = ~lat, popup = ~名称
      )
  })
  output$detailed_info <- renderUI({
    tmp <- copy(query_dt())
    tmp[, c("lng", "lat", "popup") := NULL]
    tmp2 <- as.character(tmp)
    tmp <- tmp[, which(!is.na(tmp2)), with = FALSE]
    tmp_c <- colnames(tmp)
    tmp <- paste0(tmp_c, ": ", na2blank(as.character(tmp)))
    HTML(
      paste0(vapply(tmp, function(x) as.character(p(x)), "a"), collapse = "")
    )
  })
}
