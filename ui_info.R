fluidPage(
  style = "max-width:800px",
  fluidRow(
    column(
      width = 8,
      radioButtons(
        "info_mode", NULL, 
        c("增加/Add" = "add", "修改/Modify" = "modify", "删除/Delete" = "delete"), 
        inline = TRUE)
    ),
    column(
      width = 4,
      conditionalPanel(
        "input.info_mode != 'add'",
        selectizeInput(
          "info_target",
          NULL,
          choices = NULL
        )
      )
    )
  ),
  conditionalPanel(
    "input.info_mode != 'delete'",
    bsPanel(
      theme = "default",
      header = "Info Panel",
      fluidRow(
        column(
          width = 6,
          selectizeInput("info_class", "类别/Class", 
                         choices = NULL,
                         options = list(create = TRUE))
        ),
        column(
          width = 6,
          selectizeInput("info_class_en", "英文类别/Class_EN", 
                         choices = NULL,
                         options = list(create = TRUE))
        )
      ),
      fluidRow(
        column(
          width = 6,
          textInput("info_name", "名称/Name")
        ),
        column(
          width = 6,
          textInput("info_name_en", "英文名称/Name_EN")
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectizeInput("info_area", "区域/Area", 
                         choices = NULL,
                         options = list(create = TRUE))
        ),
        column(
          width = 6,
          selectizeInput("info_country", "国家/Country", 
                         choices = NULL,
                         options = list(create = TRUE))
        )
      ),
      fluidRow(
        column(
          width = 6,
          numericInput("info_lng", "经度/LNG", 0, -180, 180, 0.01)
        ),
        column(
          width = 6,
          numericInput("info_lat", "纬度/LAT", 0, -180, 180, 0.01)
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$label(
            class = "control-label", `for` = "info_area",
            style = "display:block",
            "地址/Address"
          ),
          tags$style(type = "text/css", "textarea {width:100%}"),
          tags$textarea(id = 'info_address',
                        placeholder = 'Type your address here', rows = 2, width = "100%")
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$label(
            class = "control-label", `for` = "info_website",
            style = "display:block",
            "网址/Website"
          ),
          tags$style(type = "text/css", "textarea {width:100%; max-width:800px;}"),
          tags$textarea(id = 'info_website',
                        placeholder = 'Type your website here', rows = 2, width = "100%")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      actionButton("info_submit", "提交/Submit")
    )
  )
)
