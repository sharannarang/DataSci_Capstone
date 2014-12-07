library(shiny)
source("cleaning_function.R")
source("predict.R")
library(reshape2)
library(tm)
attach("Counts_list.RData")

shinyServer(function(input, output) {
    pred <- reactive(predict_next_word(input$input1))
    output$output1 <- renderText({pred()[1]})
}
)