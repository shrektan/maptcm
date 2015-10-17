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
  updateSelectizeInput(session, "info_class_en", selected = dt_tgt[, ClassEN])
  updateTextInput(session, "info_name", value = dt_tgt[, Name])
  updateTextInput(session, "info_name_en", value = dt_tgt[, NameEN])
  updateSelectizeInput(session, "info_area", selected = dt_tgt[, Area])
  updateSelectizeInput(session, "info_country", selected = dt_tgt[, Country])
  updateNumericInput(session, "info_lng", value = dt_tgt[, LNG])
  updateNumericInput(session, "info_lat", value = dt_tgt[, LAT])
  session$sendInputMessage("info_address", list(value = dt_tgt[, Address]))
  session$sendInputMessage("info_website", list(value = dt_tgt[, Website]))
}

# define read_info
read_info <- function() {
  data.table(
    Class = input$info_class,
    ClassEN = input$info_class_en,
    Name = input$info_name,
    NameEN = input$info_name_en,
    Area = input$info_area,
    Country = input$info_country,
    Address = input$info_address,
    Website = input$info_website,
    LNG = input$info_lng,
    LAT = input$info_lat,
    ifDeleted = FALSE,
    TimeStamp = Sys.time() %>% lubridate::with_tz(tzone = "UTC")
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
      # check if duplicate info
      new <- read_info()
      shinyjs_validate(
        need(!(new$Name %in% data()$Name), "Duplicate Chinese Name!"),
        need(!(new$NameEN %in% data()$NameEN), "Duplicate English Name!")
      )
      # rbind
      update_data(new)
      # reset info
      reset_info()
    },
    "modify" = {
      check_info()
      # check if duplicate info
      new <- read_info()
      shinyjs_validate(
        need(!(new$Name %in% data()$Name[data()$Name != input$info_target]),
             "Duplicate Chinese Name!"),
        need(!(new$NameEN %in% data()$NameEN[data()$Name != input$info_target]), 
             "Duplicate English Name!")
      )
      # delete old
      r <- data() %>% 
        dplyr::filter(Name == input$info_target) %>%
        dplyr::mutate(
          ifDeleted = TRUE,
          TimeStamp = Sys.time() %>% lubridate::with_tz("UTC")
        ) %>%
        dplyr::select(-GoTo)
      # add new
      r <- rbindlist(list(r, new), use.names = TRUE, fill = TRUE)
      update_data(r)
      # reload info
      reload_info(input$info_target)
    },
    "delete" = {
      # delete old
      r <- data() %>% 
        dplyr::filter(Name == input$info_target) %>%
        dplyr::mutate(
          ifDeleted = TRUE,
          TimeStamp = Sys.time() %>% lubridate::with_tz("UTC")
        ) %>%
        dplyr::select(-GoTo)
      update_data(r)
    }
  )
)
