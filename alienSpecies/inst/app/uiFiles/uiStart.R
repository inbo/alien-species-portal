
tagList(
  tags$div(class = "container",
    tags$div(align = "right", style= "padding-right: 15px;",
      tags$p(
        actionLink(inputId = "translate_en", label = "EN"),
        "-",
        actionLink(inputId = "translate_fr", label = "FR"),
        "-", 
        actionLink(inputId = "translate_nl", label = "NL")
      )
    )
  ),
  tags$div(class = "container", 
    align = "center", 
    tags$div(class = "noButton",
      uiOutput("start_tiles")
    )
  )
)