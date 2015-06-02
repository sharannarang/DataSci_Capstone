library(shiny)
library(reshape2)
library(tm)

options(RCHART_WIDTH = 400)
source("cleaning_function.R")
source("predict.R")

attach("Counts_list.RData")
attach("bad_words.RData")

shinyServer(function(input, output) {
    
    ## Server side implementation for tab2 [Simple prediction]
    pred <- reactive(preddf <- predict_next_word(input$input1, min_n_gram_count = 6))
    profanity_filter <- reactive(as.character(input$filter_profanities) == 'On')
    
    ## Display top output.
    output$output1 <- renderText({
        paste(input$input1, pred()[1,1], sep=" ")
    })
    
    ## Display Pie-chart of top outputs
    output$top_preds <- renderChart({
        res <- pred()
#         res$Prob <- res$Prob * 1000
        d1 <- nPlot(Prob~Pred, data=res, type='pieChart')
        d1$chart(showLegend=FALSE)
        d1$set(dom="top_preds")
        return(d1)        
    })
    
    ## Prediction for input on tab2. 
    pred1 <- reactive(pred1_res <- predict_next_word(input$input2, 
                                                     min_n_gram_count = as.numeric(input$min_n_gram_counts), 
                                                     n_grams = as.numeric(input$n_gram),
                                                     filter_profanity= profanity_filter()))

    ## Display predictions.
    output$output2 <- renderText({
        paste(input$input2, pred1()[1,1], sep=" ")
    })
    
    output$output3 <- renderText({
        pred1()[2,1]
    })
    output$output4 <- renderText({
        pred1()[3,1]
    })
    output$output5 <- renderText({
        pred1()[4,1]
    })
    output$output6 <- renderText({
        pred1()[5,1]
    })
    output$min_n_grams <- renderText({
        paste("Only N-grams occuring at least ", input$min_n_gram_counts, " times in the corpus are used for prediction", sep ="")
    })
    output$n_grams <- renderText({
        paste(input$n_gram, " words are used from the input text to predict the next word", sep="")
    })
    output$filter_profanity <- renderText({
        paste("Profanity filter is ", input$filter_profanities, sep="")
    })
}
)