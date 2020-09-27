library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)


m<-leaflet() %>% setView(lat=1.3521,lng=103.8198,zoom=11) %>% addTiles()
options <- c(1,2,3)

ui <- navbarPage(title="DBA3702 Project", id="title",
                 tabPanel(title="Interactive Map",
                          div(class="outer",
                              tags$head(includeCSS("styles.css"),
                                        includeScript("gomap.js")),
                              # ?navbarpage
                             m,
                             
                             absolutePanel(
                               id="controls", fixed=TRUE, draggable=TRUE,
                               # class="modal-body",
                               top=60,left="auto",right=20,bottom="auto",
                               width=330,height="auto",
                               h2("Title of panel"),
                               selectInput("abc","ABC",options),
                               selectInput("def","DEF",options),
                               div(style="margin: 0 auto; text-align:center;", opacity=1)
                             ))
                          ),
                 tabPanel("Secondary Schools' Data",
                                   selectInput("ghi","GHI",options
                          )
                          )
       
                 
                 )

?navbarPage()
server <- function(input, output) {
  output$text <-  renderText({
    format(Sys.time())
  })
  
}

shinyApp(ui=ui,
         server=server)
