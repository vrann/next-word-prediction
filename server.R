library(shiny)
source('ngram.R')
library(RSQLite)

db <- dbConnect(SQLite(), 'release/ngrams_nonoptimized')
unigrams = data.table(dbGetQuery(db, "SELECT * FROM unigrams"))
bigrams = data.table(dbGetQuery(db, "SELECT * FROM bigrams"))


calculatePrediction= function(sentence) {
    bestNextWordMemory(tokenize(sentence, F), unigrams, bigrams)
}

shinyServer(
    function(input, output) {
        output$sentence <- renderPrint({input$sentence})
        output$prediction = reactive({calculatePrediction(input$sentence)})
    }
)