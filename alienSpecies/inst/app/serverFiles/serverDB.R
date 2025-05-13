# Server side for DB navigation
# 
# Author: mvarewyck
###############################################################################


observeEvent(input$db_navigate, {
    
    switch(input$db_navigate, 
      "mica_db" = session$sendCustomMessage(type = "openURL", list(message = "
            window.open('https://mica.inbo.be/', '_blank').focus(); 
            ")),
      "radius_db" = session$sendCustomMessage(type = "openURL", list(message = "
            window.open('https://radius-project.shinyapps.io/dashboard/', '_blank').focus(); 
            "))
    )
    
  })
