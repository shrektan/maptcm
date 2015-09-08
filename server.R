
# data --------------------------------------------------------------------

# read data
dt <- readxl::read_excel("./data.xlsx") %>% setDT()
dt_lng_lat <- readxl::read_excel("./lng-lat.xlsx") %>% setDT()

# gen random attitude
dt[, c("lng", "lat") := dt_lng_lat[sample(1:.N, nrow(dt)), .(lng, lat)]]

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# server ------------------------------------------------------------------

function(input, output, session) {
  output$query_table <- renderDataTable({
    datatable(
      class = "compact hover row-border stripe",
      if (input$query_lang == "English") dt[, .(英文全称)] else dt[, .(名称)],
      selection = list(mode = "single", target = "row", selected = 1),
      options = list(pageLength = 5, autoWidth = FALSE,
                     dom = 'tipr', searchHighlight = TRUE),
      filter = 'top',
      rownames = TRUE
    )
  })
  output$location <- renderLeaflet({
    if (!is.null(input$query_table_rows_selected)) {
      tmp <- dt[input$query_table_rows_selected][
        , popup := paste0(p(名称), p(na2blank(英文全称)), p(na2blank(地址)), collapse = "")]
      leaflet(tmp) %>% 
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
  output$detailed_info <- renderUI({
    if (!is.null(input$query_table_rows_selected)) {
      tmp <- dt[input$query_table_rows_selected]
      tmp[, c("lng", "lat") := NULL]
      tmp2 <- as.character(tmp)
      tmp <- tmp[, which(!is.na(tmp2)), with = FALSE]
      tmp_c <- colnames(tmp)
      tmp <- paste0(tmp_c, ": ", na2blank(as.character(tmp)))
      HTML(
        paste0(vapply(tmp, function(x) as.character(p(x)), "a"), collapse = "")
      )
    }
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
