library(reshape2)

## Use string and add $ and ^ to search for exact word in the Count database
create_search_strings <- function(text, len) {
  search_string <- paste("^",text[1], "$", sep="")
  if (len != 1) {
    for (i in seq(2, len)) {
      search_string <- c(search_string, paste("^",text[i], "$", sep=""))  
    }  
  }
  search_string
}

## Find predictions for n-gram in n+1 gram count table
get_predictions <- function(words, len) {
  res <- search_freq_df(words, len, len+1, 5)
  print(res)
  res
}

## Find occurances of n-gram in n gram count table
get_count <- function(words, len) {
  res <- search_freq_df(words, len, len, 1)
  res$Count
}

## search for n gram in an n gram table
search_freq_df <- function(words, len, gram, num_results) {
  for (i in seq(1, len)) {
    if (i == 1) {
      result <- grepl(words[i], freqdf[[gram]][,i])  
      
    }
    else {
      result <- result & grepl(words[i], freqdf[[gram]][,i])
    }        
  }
  
  tail(freqdf[[gram]][result, ], num_results)
}

## Prediction function
predict_next_word <- function(text_string) {
  
  ## Clean up the input string.
  clean_text <- clean_up(text_string)
  
  ## Split the input string into words and create a vector. 
  clean_words <- unlist(strsplit(clean_text, " "))
  len <- length(clean_words)
  
  ## Still need to handle the case where the input doesn't have any text.
  mult <- 0    
  for (i in seq(4,1)) {
    if (len >= i) {
      ##  Picks last i terms
      search_words <- create_search_strings(tail(clean_words,i), i)
      
      ## Find i grams in table
      hits <- get_predictions(search_words, i)
      if (nrow(hits)) {
        ## if found, then search for the occurance of the base i-gram
        low_gram_count <- get_count(search_words, i)
        
        ## Calculate prob of prediction by dividing by base i-gram count and multiplying by back-off factor. 
        ## backoff factor 1 > 5g, 0.4 -> 4g, 0.16 -> 3g
        prob_mult <- ifelse(mult==0, 1, (0.4**mult))
        hits$Prob <- prob_mult * hits$Count/low_gram_count
        
        ## remove un-necessary columns from the data
        hits <- hits[, c(i+1, i+3)]
        colnames(hits) <- c("Pred", "Prob")
        print(hits)
        
        ## Add hits to df
        if (exists("hits_df")) {
          hits_df <- rbind(hits_df, hits, row.names=NULL)
        }
        else {
          hits_df <- hits
        }
      }
      
    }
    mult <- mult + 1
  }
  
  ## Need to handle the case where no hits are found. 
  
  hits_df <- dcast(melt(hits_df, id.vars = c("Pred")), Pred ~ ., sum)
  colnames(hits_df) <- c("Pred", "Prob")
  hits_df <- hits_df[order(hits_df$Prob, decreasing = T), ]
  as.character(head(hits_df$Pred,5))
  
}
