
# util --------------------------------------------------------------------

# na2blank
na2blank <- function(x) ifelse(is.na(x), "", x)

# data --------------------------------------------------------------------

# read data
dt <- fread("data.csv", encoding = "UTF-8")
dt_col <- fread("colname_cn.csv", encoding = "UTF-8") %>% setkey(EN)

# establish server push
server_push <- 0L

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
  if_en <- reactive(input$query_lang == "English")
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
      addTiles() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(data())) > 0.5, "navy", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~LNG, lat = ~LAT
      ) %>%
      addPopups(
        lng = ~LNG, lat = ~LAT, popup = ~popup
      ) %>% 
      setView(lng = query_dt()$LNG, lat = query_dt()$LAT, zoom = 5)
  })
  output$global_map <- renderLeaflet({
    leaflet(data()) %>% 
      addTiles() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      addCircleMarkers(
        radius = 6,
        color = ifelse(runif(nrow(data())) > 0.5, "navy", "red"),
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
  # define reset_info
  reset_info <- function() {
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
  }
  # define reload info
  reload_info <- function(info_target) {
    dt_tgt <- data() %>% dplyr::filter(Name == input$info_target)
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
  }
  # define backup data
  backup_data <- function() {
    if (!dir.exists("./csv_his")) dir.create("./csv_his")
    file.copy("data.csv", 
              file.path(
                "./csv_his", 
                paste0(format(Sys.time(), "%Y%m%d%H%M%S"),
                       basename(tempfile(pattern = "")), ".csv"))
    )
  }
  # define read_info
  read_info <- function() {
    data.table(
      Class = input$info_class,
      Class_EN = input$info_class_en,
      Name = input$info_name,
      Name_EN = input$info_name_en,
      Area = input$info_area,
      Country = input$info_country,
      Address = input$info_address,
      Website = input$info_website,
      LNG = input$info_lng,
      LAT = input$info_lat
    )
  }
  # if add then clear info; if modify then load info
  observe({
    if (input$info_mode == "add") {
      isolate({
        reset_info()
      })
    }
  })
  observe(
    if (input$info_mode == "modify") {
      reload_info(input$info_target)
    }
  )
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
  # define check_info
  check_info <- function() {
    shinyjs_validate(
      need(input$info_lng, "LNG needs a number!"),
      need(input$info_lat, "LAT needs a number!"),
      need(input$info_name, "Need a name."),
      need(input$info_name_en, "Need an english name."),
      need(input$info_class, "Need a class."),
      need(input$info_class_en, "Need an english class.")
    )
  }
  # submit info maintenance
  observeEvent(
    input$info_submit,
    switch(
      input$info_mode,
      "add" = {
        check_info()
        dt <<- rbindlist(list(dt, read_info()))
        # save csv file
        backup_data()
        readr::write_csv(dt, "data.csv", append = FALSE)
        # server push
        server_push <<- server_push + 1L
        # reset info
        reset_info()
      },
      "modify" = {
        check_info()
        # delete old
        dt <<- dt %>% dplyr::filter(Name != input$info_target)
        # add new
        dt <<- rbindlist(list(dt, read_info()))
        # save csv file
        backup_data()
        readr::write_csv(dt, "data.csv", append = FALSE)
        # server push
        server_push <<- server_push + 1L
        # reload info
        reload_info(input$info_target)
      },
      "delete" = {
        # delete old
        dt <<- dt %>% dplyr::filter(Name != input$info_target)
        # save csv file
        backup_data()
        readr::write_csv(dt, "data.csv", append = FALSE)
        # server push
        server_push <<- server_push + 1L
      }
    )
  )
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
}
