library(shiny)
library(quanteda)
library(dplyr)


source("predict function shiny.R")


shinyServer(function(input, output) {
    
    
    words1 <- reactive(predict_word(input$sentence,1))
    output$choice1<-renderText({
        words1()
    })
    words2 <- reactive(predict_word(input$sentence,2))
    output$choice2<-renderText({
        words2()
    })
    words3 <- reactive(predict_word(input$sentence,3))
    output$choice3<-renderText({
        words3()
    })
    words4 <- reactive(predict_word(input$sentence,4))
    output$choice4<-renderText({
        words4()
    })
    words5 <- reactive(predict_word(input$sentence,5))
    output$choice5<-renderText({
        words5()
    })
    
})