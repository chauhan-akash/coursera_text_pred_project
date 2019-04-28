library(shiny)
library(data.table)
library(stringr)
library(tm)
library(readr)
library(shinydashboard)

#########################################################
# Part 1: Defining functions for text pred & load data to be used by each user
#########################################################

quad_sh <- fread("./data/quadgram_freq_k4.csv", header = TRUE)
tri_sh <- fread("./data/trigram_freq_k4.csv", header = TRUE)
bi_sh <- fread("./data/bigram_freq_k4.csv", header = TRUE)
uni_sh <- fread("./data/unigram_freq_k4.csv", header = TRUE)
bw <- read_lines("./data/google_bad_words_v1.txt", skip=0)

source("./custom_func_v3.R")

#########################################################
# Part 2: The user interface function
#########################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Word Prediction App : Data Science Capstone Project",
                  titleWidth = 500),
  dashboardSidebar(disable = FALSE),
  dashboardBody(
    fluidRow(
      infoBox(
        h4("John Hopkins University - SwiftKey Collaboartion project"),
        p("Developed by Akash Chauhan - Mar 2019"),
        width = 12,
        color = "light-blue",
        icon = icon("keyboard"))
      ),
    
    h4("This app predicts the next few words based on your entered texts (English language only) 
       in the input box."),
    h4("The app uses stupid backoff algorithm to predict upto next 3 words utilizing n-gram 
       probabilities."),
    h4("The model uses 1 - 4 gram tables for prediction. These n-grams have been created
       using huge corpus of blogs, tweets and news articles."),
    br(),
  
    fluidRow(
      box(
        title = "Input Text used for prediction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        h4("Please enter your text in the box below to get the prediction for the next words"),
        textInput("inp_text", h4("To get the prediction enter a space after your text."),
                  value = "Enter text ...")
        ),
      
      box(
        title = "Predicted words",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        h4("Your next word could be one of the following words - "),
        h1(textOutput("selected_text"))
        )
      )
  )
)


#########################################################
# Part 3: The server for backend function
#########################################################

server <- function(input, output, session) {
  
  output$selected_text <- renderText({
    if(nchar(input$inp_text) > 0){
      if(tail(str_split(input$inp_text, "")[[1]],n=1) == " ")
        predict_next(input$inp_text, uni_sh, bi_sh, tri_sh, quad_sh, bw)
      else
        ""
    }
    else ""
    
  })
}

shinyApp(ui, server)
