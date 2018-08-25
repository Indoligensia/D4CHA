# https://indoligensia.shinyapps.io/ICLD/

library(shiny) 
library(shinydashboard) 

library(ROAuth)
library(streamR)
library(tidyverse)
library(leaflet)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(maps)
library(rnoaa)
library(ggplot2)
library(zoo)

library(mgcv)

# UI ----------------------------------------------------------------------

ui <- dashboardPage( 
  dashboardHeader(
    title = "#D4CHA v.0.1"
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Datasets", tabName = "dataset", icon = icon("database")),
      menuItem("Model", tabName = "upload", icon = icon("cloud-upload")),
      # menuItem("Preprocessing", tabName = "preprocess", icon = icon("table")),
      # menuItem("Model", tabName = "model", icon = icon("crosshairs")),
      menuItem("Prediction", tabName = "prediction", icon = icon("line-chart")),
      menuItem("Help", tabName = "help", icon = icon("info"))
    ),
    div(style = "padding-left: 15px; padding-top: 40px; padding-right: 15px; ",
        p(class = "small", "Disclaimer:", tags$br(),
          "We hope you will find our website and services helpful for your individual. 
          Please be aware that any result you may find may be inaccurate. Any action you take upon 
          the information on this website is strictly at your own risk, and we will not be liable 
          for any losses and damages in connection with the use of our website."))
        ), 
  
  dashboardBody(
    tabItems( 
      tabItem(tabName = "dataset", 
              h2("Datasets"),
              fluidRow(tags$head(includeScript("google-analytics.js")),
                       tabBox(width = 12, 
                              tabPanel(title = "vector",
                                       textInput("VectCountry", 
                                                 label="Input country (Ref: Kraemer et al., 2015)", 
                                                 value = "Indonesia"),
                                       actionButton("go", label = "click to create a map..."),
                                       tags$br(),tags$br(),tags$hr(),
                                       leafletOutput("map_vector"),
                                       tags$hr()),
                              #tabPanel(title = "climate"), 
                              tabPanel(title = "weather",
                                       tags$h4("study area"),
                                       numericInput("area_lat", label="latitude", value =  -7.797),
                                       numericInput("area_lon", label="longitude", value = 110.370),
                                       numericInput("area_rad", label="radius (in km)", value = 100),
                                       actionButton("go_wStatL", label = "click to search weather stations..."),
                                       tags$br(),tags$br(),tags$hr(),
                                       leafletOutput("map_wStat"),
                                       tags$hr(),
                                       tags$h4("observation period"),
                                       textInput("wStat_min", label="the earliest date (yyyy-mm-dd)", value = "2016-08-17"),
                                       textInput("wStat_max", label="the latest date (yyyy-mm-dd)", value = "2018-08-17"),
                                       actionButton("go_wStatT", label = "click to proceed"),
                                       tags$br(),tags$br(),tags$hr(),
                                       plotOutput("Wplot"),
                                       tags$hr(),
                                       tags$h4("select the observation station"),
                                       textInput("WstatID", label="weather station ID"),
                                       actionButton("go_wStatS", label = "click to select"),
                                       tags$br(),tags$br(),tags$hr(),
                                       DT::dataTableOutput('tblwStatS'),
                                       tags$hr()),
                              # tabPanel(title = "land use"),
                              # tabPanel(title = "population"),
                              # tabPanel(title = "Twitter",
                              #          tags$b("please wait..! we are generating a word cloud & map from the last 60 seconds data of Twitter streaming API"),
                              #          valueBoxOutput("n_tweets", width = 3),
                              #          #tags$br(),tags$br(),tags$hr(),
                              #          plotOutput("wc"),
                              #          tags$br(),tags$hr(),tags$br(),tags$br(),tags$br(),
                              #          plotOutput("Aplot"),
                              #          tags$hr(),tags$br(),tags$br()),
                              # tabPanel(title = "mobility",
                              #          tags$b("please wait..! the map is updated in every 60 seconds"),
                              #          tags$br(),tags$br(),tags$br(),
                              #          plotOutput("Mplot")),
                              # tabPanel(title = "news",
                              #          tags$b("GLOBAL"),tags$br(),tags$br(),
                              #          DT::dataTableOutput('tblnewsINT'),
                              #          tags$hr(),tags$br(),tags$br(),
                              #          tags$b("INDONESIA"),tags$br(),tags$br(),
                              #          DT::dataTableOutput('tblnewsLOC')),
                              # tabPanel(title = "search-term",
                              #          tags$b("Google Trends"),tags$br(),
                              #          plotOutput("Gplot")),
                              # tabPanel(title = "ProMED-mail"),
                              tabPanel(title = "++")))),
      tabItem(tabName = "upload", 
              h2("Model Development"),
              fluidRow(
                ## left
                box(width = 3, 
                    fileInput('dat_train', 'Choose file to upload',
                              accept = c('text/csv','text/comma-separated-values',
                                         'text/tab-separated-values','text/plain',
                                         '.csv','.tsv')),
                    tags$hr(),
                    checkboxInput('header', 'Header', TRUE),
                    radioButtons('sep', 'Separator',
                                 c(Comma=',',Semicolon=';',Tab='\t'),','),
                    radioButtons('quote', 'Quote',
                                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                    tags$hr(),
                    p('sample .csv file to upload:',a(href = 'https://github.com/alramadona/yews4denv/blob/master/data/dat_train.csv', 'dat_train.csv'))
                ),
                
                ## right
                box(width = 9, title = "summary",
                    plotOutput("modTPlot")
                    #tableOutput('dat_train_cont')
                )
              )),
      tabItem(tabName = "preprocess", 
              h2("Data Preprocessing"),
              fluidRow(tabBox(width = 12, 
                              tabPanel(title = "imputation"),
                              tabPanel(title = "standardization"),
                              tabPanel(title = "predictors"), 
                              tabPanel(title = "training set & validation set")))),
      tabItem(tabName = "model", 
              h2("Model Generation"),
              fluidRow(tabBox(width = 12, 
                              # tabPanel(title = "WHO model"), 
                              tabPanel(title = "Umeå model 1 (statistical)",
                                       textInput("form_text", label = h4("formula"), 
                                                 value = "dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4)"),
                                       plotOutput("modTPlot")),
                              # tabPanel(title = "Umeå model 2 (mathematical)"),
                              # tabPanel(title = "Your model (additional) "),
                              tabPanel(title = "++")))),
      tabItem(tabName = "prediction", 
              h2("Prediction"),
              fluidRow(
                ## left
                box(width = 3,
                    fileInput('dat_test', 'Choose file to upload',
                              accept = c('text/csv','text/comma-separated-values',
                                         'text/tab-separated-values','text/plain',
                                         '.csv','.tsv')),
                    tags$hr(),
                    checkboxInput('header2', 'Header', TRUE),
                    radioButtons('sep2', 'Separator',
                                 c(Comma=',',Semicolon=';',Tab='\t'),','),
                    radioButtons('quote2', 'Quote',
                                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                    tags$hr(),
                    p('sample .csv file to upload:',a(href = 'https://github.com/alramadona/yews4denv/blob/master/data/dat_test.csv', 'dat_test.csv'))
                ),
                
                ## right
                box(width = 9, title = "summary",
                    tabBox(width = 12, 
                           # tabPanel(title = "WHO model"), 
                           tabPanel(title = "Umeå model 1",
                                    plotOutput("modPPlot"),
                                    tableOutput('dat_test_cont')),
                           # tabPanel(title = "Umeå model 2"),
                           # tabPanel(title = "Your model"),
                           tabPanel(title = "++")))
              )),
      tabItem(tabName = "help", 
              h2("Help"),
              fluidRow(tabBox(width = 12, 
                              tabPanel(title = "methodology"),
                              tabPanel(title = "tutorial"), 
                              tabPanel(title = "reference",
                                       div(style = "padding-left: 15px; padding-top: 40px;",
                                           p(class = "normal", "",
                                             a("Bowman et al., 2016",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0157971"),
                                             tags$br(),
                                             a("Ramadona et al., 2016",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0152688"),
                                             tags$br()))),
                              tabPanel(title = "about us"))))
    )
  ))


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  # vector dataset
  data <- eventReactive(input$go, {
    read.csv("aegypti_albopictus.csv") %>% 
      select(VECTOR,Y,X,YEAR,COUNTRY) %>%
      filter(COUNTRY==input$VectCountry)
  })
  
  output$map_vector <- renderLeaflet({
    datMap <- data()
    
    pal <- colorFactor(palette = c('red', 'blue'),
                       domain = datMap$VECTOR)
    
    leaflet() %>% addTiles() %>% 
      addCircles(data = datMap, lat = ~ Y, lng = ~ X, color=~pal(VECTOR), fill = FALSE)
  })
  
  dat_wStatL <- eventReactive(input$go_wStatL, {
    load("datStation.RData")
    lat_lon_df <- data.frame(id="study_area", name="study_area", 
                             latitude=input$area_lat, longitude=input$area_lon) #-7.797068 #110.370529
    
    # Get all stations within 150 kilometers
    meteo_stat <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = station_data,
                                        radius = input$area_rad, var = c("PRCP","TMIN","TMAX"))
    
    meteo_stat <- meteo_stat$study_area
    meteo_stat <- rbind(lat_lon_df,meteo_stat[,c(1:4)])
    return(meteo_stat)
  })
  
  output$map_wStat <- renderLeaflet({
    datMap <- dat_wStatL()
    lat_lon_df <- datMap[1,]
    meteo_stat <- datMap[-1,]
    
    leaflet() %>% addTiles() %>%
      addCircleMarkers(data=lat_lon_df, ~longitude, ~latitude, radius = 5, color = "red", label = ~as.character(id)) %>%
      addCircleMarkers(data=meteo_stat, ~longitude, ~latitude, radius = 5, color = "blue", popup = ~as.character(id), label = ~as.character(name))
    
  })
  
  dat_wStatT <- eventReactive(input$go_wStatT, {
    datMap <- dat_wStatL()
    meteo_stat <- datMap[-1,]
    
    meteo_dat <- meteo_pull_monitors(meteo_stat$id,
                                     date_min = input$wStat_min,
                                     date_max = input$wStat_max) %>%
      rename(day = date, location = id)
    return(meteo_dat)
  })
  
  dat_wStatS <- eventReactive(input$go_wStatS, {
    meteo_datS <- meteo_pull_monitors(input$WstatID,
                                      date_min = input$wStat_min,
                                      date_max = input$wStat_max) %>%
      rename(day = date, location = id)
    
    meteo_datS <- select(meteo_datS, day,prcp,tavg) %>% mutate(tavg=(tavg/10))
    meteo_datS$YM <- as.yearmon(meteo_datS$day)
    
    meteo_datS <- select(meteo_datS, YM,prcp,tavg) %>% group_by(YM) %>%
      summarise(tavg=round(mean(tavg, na.rm=T),2),
                prcp=(sum(prcp, na.rm=T)))
    meteo_datS$YM <- as.character(meteo_datS$YM)
    return(meteo_datS)
  })
  
  output$Wplot <- renderPlot({
    meteo_dat <- dat_wStatT()
    
    meteo_plot <- meteo_dat %>%
      select(-tmax, -tmin) %>%
      tidyr::gather(parameter, value, tavg:prcp)
    
    ggplot(meteo_plot) +
      geom_line(aes(x = day, y = value, col = location)) +
      facet_grid(parameter ~ ., scales = "free_y") + theme_bw()
  })
  ##
  output$tblwStatS <- DT::renderDataTable({
    tblwStatS <- dat_wStatS()
    datatable(tblwStatS, options = list(pageLength=12))
  })
  
  # Tweets
  # RUNNING every 60 seconds
  autoInvalidate <- reactiveTimer(60000)
  
  output$Aplot <- renderPlot({
    autoInvalidate()
    datTweets <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datTweets.csv"))
    
    map("world", fill=FALSE, col="black", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
    points(datTweets$longitude, datTweets$latitude, col="red", pch=21, cex=datTweets$n/10)
  })
  
  output$n_tweets <- renderValueBox({
    autoInvalidate()
    datTweets <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datTweets.csv"))
    valueBox("# tweets", nrow(datTweets), icon = icon("twitter"), color = "purple" )
  })
  
  output$wc <- renderPlot({
    autoInvalidate()
    d <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datWC.csv"))
    
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 15,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$tblnewsINT <- DT::renderDataTable({
    
    newsINT <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/newsINT.csv"))
    newsINT$X <- NULL
    colnames(newsINT)[which(names(newsINT) == "news_date2")] <- "date"
    colnames(newsINT)[which(names(newsINT) == "news_title2")] <- "title"
    datatable(newsINT)
  })
  
  output$tblnewsLOC <- DT::renderDataTable({
    
    newsLOC <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/newsLOC.csv"))
    newsLOC$X <- NULL
    colnames(newsLOC)[which(names(newsLOC) == "news_date")] <- "date"
    colnames(newsLOC)[which(names(newsLOC) == "news_title")] <- "title"
    datatable(newsLOC)
  })
  
  output$Mplot <- renderPlot({
    autoInvalidate()
    datTweetsORI <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datTweetsORI.csv"))
    datTweetsORI <- drop_na(datTweetsORI, c(lon,lat))
    
    map("world", fill=FALSE, col="black", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
    points(datTweetsORI$lon, datTweetsORI$lat, col="red", pch=3, cex=.3)
  })
  
  output$Gplot <- renderPlot({
    load(url("http://globalminers8973.cloudapp.net:3838/d4t4/Gplot.RData"))
    datG <- res$interest_over_time
    datGden <- subset(datG, keyword=="dengue")
    datGzik <- subset(datG, keyword=="zika")
    datGchi <- subset(datG, keyword=="chikungunya")
    
    datGtot <- merge(datGden,datGzik, by="date")
    datGtot <- merge(datGtot,datGchi, by="date")
    
    plot(datGtot$date,datGtot$hits.x, ylim=c(0,100), type="l", col="red", xlab="t", ylab="hits")
    points(datGtot$date, datGtot$hits.y, type="l", col="orange")
    points(datGtot$date, datGtot$hits, type="l", col="blue")
    
    legend("topleft",
           legend=c("dengue","zika", "chikungunya"),
           pch=15, col=c("red","orange","blue"), bty="n", pt.lwd=4)
  })
  
  # output$dat_train_cont <- renderTable({
  #   inFile <- input$dat_train
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   datRaw <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  #   
  #   current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  #   random.numA <- sample(1:1000,1,replace=F) 
  #   random.numB <- sample(1001:2000,1,replace=F) 
  #   file.name <- paste("/mnt/mountpoint/datRaw/train_",current.time,"_R",random.numA,random.numB,".csv.gz", sep="")
  #   write.csv(datRaw, file=gzfile(file.name))
  #   
  #   read.csv(inFile$datapath, header = input$header,
  #            sep = input$sep, quote = input$quote)
  #   
  # })
  
  #output$value_form <- renderPrint({ input$form_text })
  
  output$modTPlot <- renderPlot({
    inFile <- input$dat_train
    datTrain <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    datTrain$dengueL2 <- lag(datTrain$dengue, n=2L)
    datTrain$rainL2 <- lag(datTrain$rain, n=2L)
    datTrain$tempL3 <- lag(datTrain$temp, n=3L)
    
    mod <- gam(denSum ~ 
                 s(dengueL2,k=4) + s(rainL2,k=4) + s(tempL3,k=4),
               data=datTrain, family=quasipoisson, na.action=na.exclude)
    
    plot(c(1:nrow(datTrain)), datTrain$dengue, type="o",
         xlab="t", ylab="cases (black)/ fitted (blue)", lwd=2)
    points(predict(modTrain, type="response"), type="l", col="blue", lwd=2)
  })
  
  output$dat_test_cont <- renderTable({
    inFile <- input$dat_train
    datTrain <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    modTrain <- gam(dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4),
                    family=quasipoisson, na.action=na.exclude, data=datTrain)
    
    inFile2 <- input$dat_test
    datTest <- read.csv(inFile2$datapath, header = input$header2, sep = input$sep2, quote = input$quote2)
    datTest$predict <- predict(modTrain, type="response", newdata=datTest)
    datTest <- datTest[,c(1:3,11)]
    return(datTest)
  })
  
  output$modPPlot <- renderPlot({
    inFile <- input$dat_train
    datTrain <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    modTrain <- gam(dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4),
                    family=quasipoisson, na.action=na.exclude, data=datTrain)
    
    inFile2 <- input$dat_test
    datTest <- read.csv(inFile2$datapath, header = input$header2, sep = input$sep2, quote = input$quote2)
    
    plot(c(1:nrow(datTest)), datTest$dengue, type="o",
         xlab="t", ylab="cases (black)/ predicted (red)", lwd=2)
    points(predict(modTrain, type="response", newdata=datTest), type="o", col="red", lwd=2)
  })
  
} 



# APP ---------------------------------------------------------------------

shinyApp(ui, server)