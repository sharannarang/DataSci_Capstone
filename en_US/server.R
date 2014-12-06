library(shiny)
source("helper.R")
load("Counts_list.RData")

shinyServer(function(input, output) {
    pred <- reactive(search_terms(input$input1))
    output$output1 <- renderText({pred()[1]})
}
)