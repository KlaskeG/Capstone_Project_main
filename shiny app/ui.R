library(shiny)
library(quanteda)
library(dplyr)



shinyUI(fluidPage(
    titlePanel("Word predictor"),
    sidebarLayout(
        sidebarPanel(
            textInput("sentence", "Type your text here", value="I am")
            
        ),
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Output", 
                                 h5("Suggestion 1"),
                                 verbatimTextOutput("choice1"),
                                 h5("Suggestion 2"),
                                 verbatimTextOutput("choice2"),
                                 h5("Suggestion 3"),
                                 verbatimTextOutput("choice3"),
                                 h5("Suggestion 4"),
                                 verbatimTextOutput("choice4"),
                                 h5("Suggestion 5"),
                                 verbatimTextOutput("choice5")
                                 
                                 ), 
                        tabPanel("Documentation", h2("Information about the word prediction app"),
                                 h5("This application predicts the next word in a text. The user can type some text. This can be a single word or multiple words. The five most likely words to follow the given words are given as suggestion."),
                                 h5("A dataset is used with data from Twitter, blogs and news messages, scraped from the internet.
The text is tokenized into 1-grams (single words) 2-grams (2 consecutive words), 3-grams and 4-grams. In this tokenizing procedure, numbers, punctuations and symbols are removed, as well as n-grams that contain words that are not English.
                                    "),
                                 h5("The dataset is not large enough to contain all the possible combination of words, also concessions had to be made to keep the processing time under control. 
Therefore, the model uses a combination of 4-grams, 3-grams and 2-grams models. If enough words are provided and the combination of the last three words is present in the dataset, the 4-gram model is used. If that is not the case a lower order model is used. 
                                    ."),
                                 h5("K. Grimmerink"),
                                 h5("02-12-2017")
                        )
            )
        )
    )
))