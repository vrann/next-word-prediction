library(shiny)
source('ngram.R')
library(RSQLite)
library(data.table)

db <- dbConnect(SQLite(), 'newrelease/ngrm')
ngrams1 = data.table(dbGetQuery(db, "SELECT * FROM unigrams"))
ngrams2 = data.table(dbGetQuery(db, "SELECT * FROM bigrams"))
ngrams3 = data.table(dbGetQuery(db, "SELECT * FROM trigrams"))
ngrams4 = data.table(dbGetQuery(db, "SELECT * FROM quadgrams"))


calculatePrediction= function(sentence) {
    #bestNextWordMemory(tokenize(sentence, F), ngrams1, ngrams2)
    getNextWords(sentence, 5)
}

shinyServer(
    function(input, output) {
        output$sentence <- renderPrint({input$sentence})
        output$prediction = reactive({calculatePrediction(input$sentence)})
    }
)