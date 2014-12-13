library(shiny)
library(reshape2)
library(tm)


options(RCHART_WIDTH = 400)
source("cleaning_function.R")
source("predict.R")

attach("Counts_list.RData")

shinyServer(function(input, output) {
    sentance <- reactive(input$input1)
    pred <- reactive(preddf <- predict_next_word(input$input1, n_grams=2))
    output$output1 <- renderText({
        paste(input$input1, pred()[1,1], sep=" ")
    })
    output$top_preds <- renderChart({
#         barplot(pred()[,2], names.arg = pred()[,1])
#         d1 <- dPlot(y="Prob", x="Pred", data=pred(), type="bar")

        d1 <- nPlot(Prob~Pred, data=pred(), type='pieChart')
        d1$chart(showLegend=FALSE)
        d1$set(dom="top_preds")
        return(d1)        
    })
#     output$output2 <- renderText({pred()[1][2]})
#     output$output3 <- renderText({pred()[3]})
#     output$output4 <- renderText({pred()[4]})
#     output$output5 <- renderText({pred()[5]})
}
)