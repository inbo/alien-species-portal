
tagList(
  tags$div(align = "right",
    tags$p(
      actionLink(inputId = "translate_en", label = "EN"),
      "-",
      actionLink(inputId = "translate_fr", label = "FR"),
      "-", 
      actionLink(inputId = "translate_nl", label = "NL")
    )
  ),
  tags$div(class = "container", 
    align = "center", 
    tags$div(class = "noButton",
      uiOutput("start_tiles")
    )
  ),
  uiOutput("start_disclaimer")
)