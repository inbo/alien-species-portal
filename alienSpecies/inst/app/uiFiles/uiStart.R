tagList(
  tags$div(class = "front-hero-wrapper",
           
           # Full-width background slideshow
           slickR::slickROutput("hero_carousel", width = "100%", height = "100%"),
           
           # Dark gradient overlay for contrast
           tags$div(class = "front-hero-overlay"),
           
           # Foreground content: welcome header + bottom row of tiles
           tags$div(class = "front-hero-content",
                    
                    tags$div( class = "front-welcome-box",
                              tags$h1(class = "front-welcome-title",
                                      translate("welcome")$title),
                              tags$p(class = "front-welcome-subtitle",
                                     translate("welcome")$description)),
                    
                    tags$div(class = "front-bottom-tiles noButton",
                             uiOutput("start_tiles"))
           )
  ),
  
  tags$footer(class = "bottom-banner",
              tags$a(href = "https://osf.io/7dpgr/", target = "_blank",
                     tags$img(src = "www/logoTrias.png", style = "height:50px;")),
              tags$a(href = "https://b-cubed.eu/", target = "_blank",
                     tags$img(src = "www/bcubed.png", style = "height:50px;"))
              )
)