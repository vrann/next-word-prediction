library(shiny)
#library(twitteR)
#library(textcat)
library(RSQLite)
library(data.table)

library(wordcloud)
#install_github("ShinySky", username="vrann")

library(shinysky)
source('ngram.R')

calculatePrediction= function(sentence) {
    getNextWords(sentence, 5)$V1
}

shinyServer(
    function(input, output, session) {
        output$simpleSentence <- renderPrint({input$simpleSentence})
        output$prediction = reactive({termsSimple()[1]$V1})
        #output$plot1 <- renderPlot({
        #    nextWords = termsSimple()
        #    wordcloud(nextWords$V1, nextWords$size,
        #              scale=c(4, 0.5),
        #              colors=brewer.pal(8, "Dark2"))
        #})
        
        output$plot2 <- renderPlot({
            nextWords = terms()
            wordcloud(nextWords$V1, nextWords$size,
                      scale=c(4, 0.5),
                      colors=brewer.pal(8, "Dark2"))
        })
        
        
        termsSimple <- reactive({
            nextWords =  getNextWords(paste(input$simpleSentence, collapse = " "), 40)
        })
        terms <- reactive({
            nextWords =  getNextWords(paste(input$sentence, collapse = " "), 40)
        })
        
        
        observe({
            nextWords = terms()[1:7]$V1
            updateSelect2Input(session, "sentence", choices = nextWords, selected = input$sentence, label = "hello")
        })
        
        
    }
)