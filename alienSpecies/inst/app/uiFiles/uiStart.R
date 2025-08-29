
tagList(
  tags$div(class = "container", 
    align = "center", 
    tags$div(class = "noButton",
      uiOutput("start_tiles")
    )
  ),
  
  tags$footer(class = "bottom-banner",
    tags$a(href = "https://osf.io/7dpgr/", target = "_blank", 
      tags$img(src = "www/logoTrias.png", style = "height:50px;"))
  )

)