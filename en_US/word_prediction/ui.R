library(shiny)
require(rCharts)

shinyUI(navbarPage("Word Prediction Application",
            tabPanel("Algorithm and Application Details",
                h2("How to use the Application"),                   
                p("The application contains two modes:"),
                tags$ul(tags$li("Simple Prediction Mode: In this mode, the application takes the input string and generates the top prediction alongwith the probabilities of the top 5 predictions."),
                        tags$li("Custom Prediction Mode: In this mode, you can customize the parameters of the application to see the resulting prediction. The size of the model and   number of input words to consider can be varied to make the prediction. The profanity filter can also be controlled in this mode.")),
                p("After clicking on the tabs, please wait for the application to initialize and complete the default prediction.", 
                  br(),
                  "For grading, please use the Simple Word Prediction Mode. The Custom Mode is more for experimentation and fun."),
                h2("Algorithm"),
                h4("Input processing"),
                p("The algorithm accepts the input string and performs the following steps to clean the text:"),
                tags$ol(tags$li("Replace all different type of quotations and apostrophes with the simple single quote."),
                        tags$li("Remove all punctuations besides single quotation marks."), 
                        tags$li("Replace all characters that are not decoded correctly."),
                        tags$li("Remove all characters that aren't alphabets, spaces or single quotes"),
                        tags$li("convert all characters to lowercase"),
                        tags$li("Perform more operations on single quotes like remove quotes from the end/beginning of words, replace it's with it is, remove all 's from the corpus."),
                        tags$li("Remove any remaining whitespace")
                ),
                h4("Predictive Model"),
                p("In order to predict the next word in any sentance, a corpus was built using the twitter, blogs and news datasets.
                   After cleaning the dataset, the corpus was broken down into n-grams where n = 1,2,3,4,5. 
                   N-gram counts are computed by creating DocumentTermMatrices. The model saves upto 5 grams. Finalize Any n-gram occuring less than 5 times is discared."),
                h4("Prediction Algorithm"),
                p("In order to predict the next word, the algorithm uses the Stupid Backoff technique. 
                  After cleaning the input text, the algorithm picks the last four words and looks for a match in the 5 gram model. If a match is found, the probability of the n-gram is computed and saved. Probabilities are computed by simply diving the N gram count by the (N-1) gram count."),
                p("The algorithm drops the leading word and searches the remaining 3 words in the 4 gram model. If a match is found, the probability is computed and scaled down with a backoff facter (0.4) and saved.
                   This process is repeated for the remaining 2 words. However, The probability from the 3 gram model is scaled down using a larger backoff factor (0.16). 
                   This same process is applied for the 2 gram model with an even larger backoff factor.", br(), 
                   "Finally, the results are combined and the predictions with highest probabilites are returned."),       
                h4("Final Model statistics"),
                tags$ul(tags$li("Model Size: 34 MB"),
                        tags$li("Model Accuracy: 11.67%"),
                        tags$li("Average Response Time: 4 seconds")),
                h2("Acknowledgements"),
                p("I would like to thank the JHU Course staff for a great learning experience in the Data Science Specialization.", 
                  br(),
                  "The Stanford NLP lecture vidoes were very helpful in getting started in the field of NLP.",
                  br(),
                  "Finally, it would not have been possbile to complete the series without the assistance of the Community TAs and fellow Students. Thank you very much!")        
            ),
            tabPanel("Simple Word Prediction",
                sidebarLayout(
                    sidebarPanel(
                        textInput("input1", "Enter any incomplete sentance", "Where do we go from"),
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
                        h4("Top Predicted Words with Probabilities:"),
                        showOutput("top_preds", "nvd3")
                    )
                )
            ),
            tabPanel("Custom Word Prediction",
                     sidebarLayout(
                         sidebarPanel(
                             textInput("input2", "Enter any incomplete sentance", "All we need is"),
                             selectInput("n_gram", "Number of words used in Prediction", c(1,2,3,4), selected = 4),
                             radioButtons("filter_profanities", "Filter Profanities", c("On", "Off"), selected = "On"),
                             selectInput("min_n_gram_counts", "Minimum N-gram Counts in model", c(seq(2,10)), selected = 2),                             
                             submitButton("submit")                                    
                         ),
                         mainPanel(
                             p("This mode allows the user to customize the following parameters:"),
                             tags$ol(tags$li("Number of words used in prediction: This parameter indicates how many of the input words are used while predicting the next word. This can be varied from 1 to 4."),
                                     tags$li("Minimum N-gram Counts in model: This parameter allows the user to control the size of the model. As this parameter is increased, the size of the model reduces since it ignores more N-grams and accuracy reduces. However, the response time increases as well."),
                                     tags$li("Filter Profanities: With this switch, the user can enable or disable the profanity filter.")),
                             p("After selecting the parameters, please enter any sentance and hit the submit button."),
                             h4("Your selections:"),
                             textOutput('min_n_grams'),
                             textOutput('filter_profanity'),
                             textOutput('n_grams'),
                             h4("Prediction:"),
                             h4(div(textOutput('output2'), style = "color:blue" )),
                             br(),
                             h5("Other Top Predictions:"),
                             textOutput("output3"),
                             textOutput("output4"),
                             textOutput("output5"),
                             textOutput("output6")
                        )
                     )
            )
))