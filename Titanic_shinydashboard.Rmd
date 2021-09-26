---
title: "Survival Predictor for Titanic"
output: html_document
runtime: shiny
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(wordcloud2)
library(tm)
library(xml2)
library(rvest)
library(dbplyr)

library(shinythemes)
library(data.table)
library(randomForest)
```

```{r include=FALSE}
model <- readRDS("model.rds")
titanic.fix.train <- read.csv("train_shiny.csv")
train <- titanic.fix.train
```

```{r include=FALSE}
#str(titanic.fix.train)
titanic.fix.train$Survived <- as.factor(titanic.fix.train$Survived)
titanic.fix.train$Pclass <- as.factor(titanic.fix.train$Pclass)
titanic.fix.train$Cabin.woN <- as.factor(titanic.fix.train$Cabin.woN)
titanic.fix.train$Title <- as.factor(titanic.fix.train$Title)
#str(titanic.fix.train)
```

```{r warning=FALSE, include=FALSE}
ui <- dashboardPage(skin = "black", 
  dashboardHeader(title = "Survival Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Survival Predictor", tabName = "predictor", icon = icon("sos")),
      selectInput(inputId = "Cabin.woN", label = "Cabin",
                  choices = model$forest$xlevels$Cabin.woN,
                  selected = "A"),
      selectInput(inputId = "Sex", label = "Gender",
                  choices = list("Female" = "female", "Male" = "male"),
                  selected = "Female"),
      selectInput(inputId = "Title", label = "Title",
                  choices = model$forest$xlevels$Title,
                  selected = "Miss."),
      br(),
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
      
    )),
    
  dashboardBody(
    tabItems(
      tabItem("predictor",
              h1("Survival Predictor"),
              tableOutput("predictor")
              #plotOutput("pie")
              )
    )
  )
 )


```


```{r warning=FALSE, include=FALSE}
server <- function(input, output){
  datasetInput <- reactive({
        
        df <- data.frame(
            Name = c("Cabin.woN",
                     "Sex",
                     "Title"
                     ),
            Value = as.character(c(input$Cabin.woN,
                                   input$Sex,
                                   input$Title)),
            stringsAsFactors = FALSE)
        
        Survived <- 0
        df <- rbind(df, Survived)
        input <- transpose(df)
        write.csv(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input",".csv", sep=""), header = TRUE)
        colnames(test) <- c(test[1,1:3],"Survived")
        test <- test[-1,]
        test$Cabin.woN <- as.factor(test$Cabin.woN)
        test$Cabin.woN <- factor(test$Cabin.woN, model$forest$xlevels$Cabin.woN)
        
        test$Sex <- as.factor(test$Sex)
        test$Sex <- factor(test$Sex, model$forest$xlevels$Sex)
        
        test$Title <- as.factor(test$Title)
        test$Title <- factor(test$Title, model$forest$xlevels$Title)
        
        test$Survived <- as.factor(test$Survived)
        test$Survived <- factor(test$Survived, c("0","1"))
        

        Output <- data.frame(Prediction=predict(model, test, type = "response"), 
                             round(predict(model,test, type="prob"),3))
        Output <- Output[,-1]
        colnames(Output) <- c("Dead","Survived")
        print(Output)
        
        
    })
   

 
   
   
 output$predictor <-  renderTable({
        if (input$submitbutton>0){
            isolate(datasetInput())
        }
    })

# Try to draw a pie chart 
# output$pie <- renderPlot({
#  pie(isolate(datasetInput())[1,])
# })

} 
```


```{r echo=FALSE, warning=FALSE}
shinyApp(ui = ui, server = server, options = list(height=1300))
```
