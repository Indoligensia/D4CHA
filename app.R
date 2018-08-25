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
    title = "YogyEWS v.0.1"
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Training Data", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Model Development", tabName = "model", icon = icon("crosshairs")),
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
      tabItem(tabName = "upload", 
              h2("upload"),
              tags$head(includeScript("google-analytics.js")),
              fluidRow(
                ## left
                box(width = 12, 
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
                ))),
      tabItem(tabName = "model", 
              h2("model"),
              fluidRow(tabBox(width = 12, 
                              tabPanel(title = "++",
                                       textInput("form_text", label = h4("formula"), width = '100%',
                                                 value = "dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4)"),
                                       plotOutput("modTPlot")),
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
                           tabPanel(title = "++",
                                    plotOutput("modPPlot"),
                                    tableOutput('dat_test_cont')),
                           tabPanel(title = "++")))
              )),
      tabItem(tabName = "help", 
              h2("Help"),
              fluidRow(tabBox(width = 12, 
                              #tabPanel(title = "methodology"),
                              #tabPanel(title = "tutorial"), 
                              tabPanel(title = "reference",
                                       div(style = "padding-left: 15px; padding-top: 40px;",
                                           p(class = "normal", "",
                                             a("Ramadona AL, et al. PLoS One. 2016",href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0152688"),
                                             tags$br()))),
                              tabPanel(title = "about us"))))
    )
  ))


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  output$modTPlot <- renderPlot({
    inFile <- input$dat_train
    datTrain <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    modTrain <- gam(dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4),
                    family=quasipoisson, na.action=na.exclude, data=datTrain)
    
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
    datTest <- datTest[,c(1:2,11)]
    names(datTest) <- c("year", "month", "prediction")
    return(datTest)
  })
  
  output$modPPlot <- renderPlot({
    inFile <- input$dat_train
    datTrain <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    modTrain <- gam(dengue ~ s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4) + s(denguel2,k=4) + s(denguel24,k=4),
                    family=quasipoisson, na.action=na.exclude, data=datTrain)
    
    inFile2 <- input$dat_test
    datTest <- read.csv(inFile2$datapath, header = input$header2, sep = input$sep2, quote = input$quote2)
    
    # plot(c(1:nrow(datTest)), datTest$dengue, type="o",
    #      xlab="t", ylab="cases (black)/ predicted (red)", lwd=2)
    # points(predict(modTrain, type="response", newdata=datTest), type="o", col="red", lwd=2)
    
    plot(c(1:nrow(datTest)), predict(modTrain, type="response", newdata=datTest), type="o", col="red", 
         xlab="t", ylab="cases (black)/ predicted (red)", lwd=2)
    })
  
} 



# APP ---------------------------------------------------------------------

shinyApp(ui, server)