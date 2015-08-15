library(shiny)
shinyUI(
    pageWithSidebar(
        headerPanel("Next Word Prediction"),
        sidebarPanel(
            textInput('sentence', "What's on your mind"),
            verbatimTextOutput("prediction"),
            submitButton('Submit')
            
        ),
        mainPanel(
            h3('Instructions'),
            p('This is an application predicts next entered word based on the context already entered.
              It uses twitter, blogs and news corpora as training set')
            
        )
    )
)