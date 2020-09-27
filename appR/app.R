library(dplyr)
library(zipcode)
library(RCurl)
library(zipcode)
library(ggthemes)

x <- getURL("https://raw.githubusercontent.com/majerus/college_map/master/data/superzip.csv")
allzips <- read.csv(text = x)

#allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
#allzips$college <- allzips$college * 100


allzips$zip <- clean.zipcodes(allzips$HD2013.ZIP.code)


row.names(allzips) <- allzips$unitid

allzips$state.x <- ifelse(is.na(allzips$state.x) & allzips$city.x=='Washington', 'D.C.', as.character(allzips$state.x))  # make DC a state so it is not missing from analysis

# transform variables 
allzips$l_enorllment <- log(allzips$DRVEF122013.12.month.full.time.equivalent.enrollment..2012.13 + 1)
allzips$ID <- allzips$zipcode

allzips$city.x <- as.character(allzips$city.x)

cleantable <- allzips %>%
  select(
    Institution = institution.name, 
    City = city.x,
    State = state.x,
    Zipcode = zip,
    #Rank = rank,
    Tuition = centile,
    #Superzip = superzip,
    Enrollment = adultpop,
    Selectivity = college,
    Applications = income,
    Lat = latitude,
    Long = longitude, 
    ID = zipcode
  )


# handle missing data 
allzips$college <- ifelse(is.na(allzips$college), 101, allzips$college)  # missing admit rates to 101 so they work with adjustable selectivity 
allzips[is.na(allzips)] <- 0  # all other missing values to 0 
allzips <- na.omit(allzips) # rows that are still missing values are dropped 




# temporarily remove colleges with missing data 
# cleantable <- na.omit(cleantable)



library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Selectivity (adjustable)" = "superzip",
  "Tuition and Fees" = "centile",
  "Selectivity" = "college",
  "Applicants" = "income",
  "Enrollment" = "adultpop", 
  "log(Enrollment)" = "l_enorllment"
)





shinyUI(navbarPage("Four-Year Not-for-Profit Colleges", id="nav", 
                   
                   tabPanel("Interactive map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletMap("map", width="100%", height="100%",
                                           initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                           initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                           options=list(
                                             center = c(37.45, -93.85),
                                             zoom = 5,
                                             maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
                                           )
                                ),
                                
                                absolutePanel(
                                  id = "controls", 
                                  #class = "modal-body", 
                                  fixed = TRUE, draggable = TRUE,
                                  top = 60, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h2("College Explorer"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "Selectivity threshold (admit rate less than)", 10)
                                  ),
                                  
                                  plotOutput("histCentile", height = 200),
                                  plotOutput("scatterCollegeIncome", height = 250), 
                                  #img(src="TCA_Logo_K (2).png", width = 150))
                                  div(style = "margin: 0 auto;text-align: center;", 
                                      HTML("</br> 
                                           <a href=http://www.thirdcoastanalytics.com target=_blank>
                                           <img style='width: 150px;' src='http://i.imgur.com/ZZkas87.png'/> </a>"))
                                  #        http://imgur.com/kBhR1Q8
                                  #        http://i.imgur.com/TfGZPss.png
                                  #[Imgur](http://i.imgur.com/ZZkas87.png)
                                  #tags$a(href="www.rstudio.com", "Click here!")
                                      ),
                                
                                tags$div(id="cite",
                                         HTML('Contact <a href="http://www.richmajerus.com/" target="_blank" >Rich Majerus </a> (rich.majerus@gmail.com) with questions, comments or concerns.  This application was built from code developed by the <a href="https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example" target="_blank" >RStudio Team</a>. Data Source: National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) 2012-2013.  Due to these data being self-reported by each institution, the quality of the data in this visualization is only as high as the quality of institutional reporting. This visualization presents IPEDS data “as is." '
                                         ))
                                  )
                   ),
                   
                   
                   tabPanel("Data explorer",
                            fluidRow(
                              column(3,
                                     selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                     )
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                     )
                              )
                            ),
                            #fluidRow(
                            #  column(5,
                            #    numericInput("minScore", "Min Tuition", min=0, max=50000, value=0)
                            #  ),
                            #  column(5,
                            #    numericInput("maxScore", "Max Tuition", min=0, max=50000, value=50000)
                            #  )
                            #),
                            hr(),
                            dataTableOutput("ziptable")
                   ),
                   
                   conditionalPanel("false", icon("crosshair"))
                   
                   # HTML("<i class=fa fa-crosshairs></i>")
                   
))
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

#removed for college analysis
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
zipdata <- allzips

# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  map <- createLeafletMap(session, "map")
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    
    hist(zipsInBounds()$centile[zipsInBounds()$centile!=0],
         breaks = centileBreaks,
         main = "Tuition and Fees (visible zips)",
         xlab = "Tuition and Fees",
         xlim = range(allzips$centile, na.rm=T),
         col = '#00DD00',
         border = 'white')
  })
  
  
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(centile ~ college, data = subset(zipsInBounds(), zipsInBounds()$centile!=0 & zipsInBounds()$college!=0),
                 xlim = range(allzips$college), ylim = range(allzips$centile), 
                 xlab = "Admit Rate", ylab = "Tuition"))
  })
  
  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      colorData <- if (colorBy == "superzip") {
        as.numeric(allzips$college < (input$threshold))
      } else {
        allzips[[colorBy]]
      }
      colors <- brewer.pal(7, "Spectral")[cut(colorData, 7, labels = FALSE)]
      colors <- colors[match(zipdata$zipcode, allzips$zipcode)]
      
      # Clear existing circles before drawing
      map$clearShapes()
      # Draw in batches of 1000; makes the app feel a bit more responsive
      chunksize <- 1000
      for (from in seq.int(1, nrow(zipdata), chunksize)) {
        to <- min(nrow(zipdata), from + chunksize)
        zipchunk <- zipdata[from:to,]
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
        try(
          map$addCircle(
            zipchunk$latitude, zipchunk$longitude,
            (zipchunk[[sizeBy]] / max(allzips[[sizeBy]])) * 30000,
            zipchunk$zipcode,
            list(stroke=FALSE, fill=TRUE, fillOpacity=0.4),
            list(color = colors[from:to])
          )
        )
      }
    })
    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(df, zipcode, lat, lng) {
    selectedZip <- df[df$zipcode == zipcode,]
    selectedZip$income <- ifelse(selectedZip$income==0, NA, selectedZip$income)
    selectedZip$college <- ifelse(selectedZip$college==101, NA, selectedZip$college)
    selectedZip$adultpop <- ifelse(selectedZip$adultpop==0, NA, selectedZip$adultpop)
    selectedZip$centile <- ifelse(selectedZip$centile==0, NA, selectedZip$centile)
    
    content <- as.character(tagList(
      #tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$h4(selectedZip$institution.name),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedZip$city.x, selectedZip$state.x
      ))), tags$br(),
      
      
      if(!(is.na(selectedZip$income))){
        sprintf("Applications: %s",   format(as.integer(selectedZip$income), big.mark=",",scientific=F))}, tags$br(),
      
      if(!(is.na(selectedZip$college))){
        sprintf("Admit Rate: %s%%", as.integer(selectedZip$college))}, tags$br(),
      
      if(!(is.na(selectedZip$adultpop))){
        sprintf("Enrollment: %s", format(as.integer(selectedZip$adultpop), big.mark=",",scientific=F))}, tags$br(),
      
      if(!(is.na(selectedZip$centile))){
        sprintf("Tuition and Fees: %s", paste('$', format(as.integer(selectedZip$centile), big.mark=",",scientific=F), sep=''))}
      
    ))
    map$showPopup(lat, lng, content, zipcode)
  }
  
  
  # When map is clicked, show a popup with city info
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(allzips, event$id, event$lat, event$lng)
    })
  })
  
  session$onSessionEnded(clickObs$suspend)
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %.%
        `$`('City') %.%
        unique() %.%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %.%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %.%
        `$`('Zipcode') %.%
        unique() %.%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map$clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      #showZipcodePopup(allzips, zipcode, allzips$latitude, allzips$longitude)
      map$fitBounds(lat - dist, lng - dist,
                    lat + dist, lng + dist)
    })
  })
  
  output$ziptable <- renderDataTable({
    cleantable %>%
      filter(
        #Score >= input$minScore,
        #Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  }, escape = FALSE)
})




