library(shiny)
#library(devtools)
#install_github("vrann/ShinySky")
library(shinysky)

shinyUI(
    fluidPage(
        fluidRow(
            column(2, align="left"),
            column(8, align="center",
                   
        headerPanel("Next Word Prediction"),
        sidebarLayout(
            sidebarPanel(
                tabsetPanel(
                    tabPanel("Basic Mode", 
                             p('The application predicts next word you are most likely to type.
                               Start typing. The textbox under the input field will show the predicted word.'),
                             textInput('simpleSentence', ""),
                             tags$head(tags$style(type="text/css", "#simpleSentence {width: 95%}")),
                             verbatimTextOutput("prediction"),
                             tags$head(tags$style(type="text/css", "#prediction {width: 94%; font-size: 26px}"))
                             , 
                             hr()
                             
                             #plotOutput("plot1", width = "96%")
                    ),
                    tabPanel("Advanced Mode", 
                             p('This section of application shows the next 8 best words which can be added to the input field 
with a single click. 
Start typing. Dropdown list will show you the words. Click on the word in order to add to list. 
If no words found satisfiable, continue typing.'),
                             select2Input("sentence","",
                                          choices=c(),
                                          selected=c()
                             ),
                             tags$head(tags$style(type="text/css", "#sentence {width: 95%}")),  hr(),                         
                             plotOutput("plot2", width = "96%")
                             
                    ),
                    tabPanel("How it works", 
                             p('Application predicts next word to enter based on the already entered words.
                         It uses twitter, blogs and news corpora as a training set.', align='left'),
                             
                             p('It calculates how many times each word from the training set appears in the training set. 
                            After that it builds sequences of two words, 3 words, etc, up to 5 words and calculates how often each sequence 
                               appears in the text.', align='left'),
                             
                             p('When that is done, application builds predictive model using Kneser-Ney smoothing algorithm, 
                               calculating probabilities of every word and every sequence. 
                               All this information is stored in the database.', align='left'),
                             
                             p('When user enters the text in the input field, application takes the input and searches among the previously built sequences to find those, 
where input is a subsequence. Then it just picks the one with the highest probability and displays to user the tail of the sequence. Voila!', align='left')
                             
                             
                    )
                ),              
                width="50%"
                
            ),    
        
        mainPanel(
            
            
            
       )
        )
            ),
       column(2, align="right")
       
       )
    )
)    
