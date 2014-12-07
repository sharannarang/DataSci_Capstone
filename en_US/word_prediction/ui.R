library(shiny)


shinyUI(pageWithSidebar(
  headerPanel("Prediction Game"),
  sidebarPanel(
    textInput("input1", "Input Word", "Let's go"),
    submitButton("submit")
    ),
  
  mainPanel(
    textOutput('output1')
    )
))