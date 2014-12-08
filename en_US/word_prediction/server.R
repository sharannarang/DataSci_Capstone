library(shiny)
library(reshape2)
library(tm)

source("cleaning_function.R")
source("predict.R")

attach("Counts_list.RData")

shinyServer(function(input, output) {
    pred <- reactive(predict_next_word(input$input1))
    output$output1 <- renderText({pred()[1]})
}
)