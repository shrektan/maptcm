
function(input, output, session) {
  output$query_table <- renderDataTable({
    datatable(
      dt, 
      selection = list(mode = "single", target = "row", selected = 1),
      options = list(pageLength = 5, scrollX = TRUE, autoWidth = FALSE)
    )
  })
  output$location <- renderLeaflet({
    if (!is.null(input$query_table_rows_selected)) {
      tmp <- dt[input$query_table_rows_selected]
      leaflet(tmp) %>% 
        addTiles() %>%
        addProviderTiles("OpenStreetMap.HOT") %>%
        addCircleMarkers(
          radius = 6,
          color = ifelse(runif(nrow(dt)) > 0.5, "navy", "red"),
          stroke = FALSE, fillOpacity = 0.5,
          lng = ~lng, lat = ~lat, popup = ~名称
        ) %>% 
        setView(lng = tmp$lng, lat = tmp$lat, zoom = 5)
    }
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
}


# tmp <- dt[1:5, .(名称, lng, lat)]
# pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
# 
# leaflet(dt) %>% 
#   addTiles() %>%
#   addProviderTiles("OpenStreetMap.HOT") %>%
#   addCircleMarkers(
#     radius = 6,
#     color = ifelse(runif(nrow(dt)) > 0.5, "navy", "red"),
#     stroke = FALSE, fillOpacity = 0.5,
#     lng = ~lng, lat = ~lat, popup = ~名称
#   )
#   addMarkers(~lng, ~lat, icon = greenLeafIcon, popup = ~名称)
#   addCircles(lng = ~lng, lat = ~lat, popup = ~名称)
# 
# 
# greenLeafIcon <- makeIcon(
#   iconUrl = "http://leafletjs.com/docs/images/leaf-green.png",
#   iconWidth = 38/3, iconHeight = 95/3
# )
# 
# leaflet(data = quakes[1:4,]) %>% addTiles() %>%
#   addMarkers(~long, ~lat, icon = greenLeafIcon)
