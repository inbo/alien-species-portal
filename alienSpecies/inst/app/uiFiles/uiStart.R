
tagList(
  
  tags$div(class = "container", 
    
    align = "center", 
    tags$div(class = "noButton",
      radioButtons(inputId = "start_navigate", label = "", inline = TRUE,
        choiceValues = list("Start", "Global Indicators", "Species Information", 
          "Early Warning?", "Management?"),
        choiceNames = list(
          "",
          HTML("<div class='fotoTitel'>Indicators</div><div id='exampleFoto'></div>"),
          HTML("<div class='fotoTitel'>Species</div><div id='exampleFoto'></div>"),
          HTML("<div class='fotoTitel'>Early Warning</div><div id='exampleFoto'></div>"),
          HTML("<div class='fotoTitel'>Management</div><div id='exampleFoto'></div>"))
      )
    )
  )
)