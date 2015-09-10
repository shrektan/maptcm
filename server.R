
# data --------------------------------------------------------------------

# read data
dt <- readr::read_csv("data.csv") %>% setDT()

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# server ------------------------------------------------------------------

function(input, output, session) {
  output$query_table <- renderDataTable({
    datatable(
      class = "compact hover row-border stripe",
      if (input$query_lang == "English") dt[, .(英文名称)] else dt[, .(名称)],
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
        , popup := paste0(p(名称), p(na2blank(英文名称)), p(na2blank(地址)), collapse = "")]
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
