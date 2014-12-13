library(shiny)
require(rCharts)

shinyUI(pageWithSidebar(
  headerPanel("Word Prediction App"),
  sidebarPanel(
      textInput("input1", "Enter any incomplete sentance", "All we need"),
      submitButton("submit")
    ),
  
  mainPanel(
    p("This application predicts the next word of any given partial sentance. In order to use the app, enter any partial sentance in the text box on the left.
       There is no restriction on the length of the sentance.
       After processing the input, the application will generate the top prediction alongwith a piechart representing the probabilities of the top 5 predicted words."),
    br(),
    h4("Prediction:"),
    h4(div(textOutput('output1'), style = "color:blue" )),
    br(),
    br(),
    h4("Top 5 Predicted Words with Probabilities:"),
    showOutput("top_preds", "nvd3")
    )
))