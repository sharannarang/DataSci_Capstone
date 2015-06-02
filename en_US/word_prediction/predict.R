library(reshape2)

default_min_n_gram_count <- 2

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
get_predictions <- function(words, len, count_list) {
  res <- search_freq_df(words, len, len+1, 5, count_list)
  #print(res)
  res
}

## Find occurances of n-gram in n gram count table
get_count <- function(words, len, count_list) {
  res <- search_freq_df(words, len, len, 1, count_list)
  res$Count
}

## search for n gram in an n gram table
search_freq_df <- function(words, len, gram, num_results, count_list) {
#   if (min_n_gram_count != default_min_n_gram_count) {
#       freqdf_mod <- lapply(freqdf, FUN=function(x) x[x$Count > min_n_gram_count,])
#       use_mod_df <- 1
#   }
#   else {
#       use_mod_df <- 0
#   }
  for (i in seq(1, len)) {
    if (i == 1) {
        result <- grepl(words[i], count_list[[gram]][,i])
    }
    else {
        result <- result & grepl(words[i], count_list[[gram]][,i])
    }        
  }
  tail(count_list[[gram]][result,], num_results)
}

## Prediction function
## text_string -> Input string
## clean_text -> Whether the input text needs to be cleaned.
## num_preds -> Number of predictions to be returned
## n_grams -> How many max words to consider for prediction (1,2,3,4)
## min_n_gram_count -> Controls model size and prediction time. Ignores all n_grams with counts < min_n_gram_count. 
## filter profanity -> remove profanities from the predictions
predict_next_word <- function(text_string, clean_text=1, num_preds=5, n_grams=4, min_n_gram_count=2, filter_profanity=T) {
  
  if (clean_text) {
    ## Clean up the input string.
    clean_text <- clean_up(text_string)
    
    ## Split the input string into words and create a vector. 
    clean_words <- unlist(strsplit(clean_text, " "))    
  }
  else {
    clean_words <- text_string
  }

  len <- length(clean_words)
  
  if (min_n_gram_count != default_min_n_gram_count) {
      count_list_mod <- lapply(freqdf, FUN = function(x) x[x$Count>=min_n_gram_count,])
  }
  
  mult <- 0    
  ## Find matches for n_grams with n starting from n_grams specified in the input to 1 gram.
  for (i in seq(n_grams,1)) {
    if (len >= i) {
      ##  Picks last i terms
      search_words <- create_search_strings(tail(clean_words,i), i)
      
      ## Find i grams in table
      if (min_n_gram_count != default_min_n_gram_count) {
          hits <- get_predictions(search_words, i, count_list_mod)    
      }
      else {
          hits <- get_predictions(search_words, i, freqdf)
      }
      
      if (nrow(hits)) {
        ## if found, then search for the occurance of the base i-gram
        if (min_n_gram_count != default_min_n_gram_count){
            low_gram_count <- get_count(search_words, i, count_list_mod)
        }
        else {
            low_gram_count <- get_count(search_words, i, freqdf)    
        }
        ## Calculate prob of prediction by dividing by base i-gram count and multiplying by back-off factor. 
        ## backoff factor 1 > 5g, 0.4 -> 4g, 0.16 -> 3g
        prob_mult <- ifelse(mult==0, 1, (0.4**mult))
        hits$Prob <- prob_mult * hits$Count/low_gram_count
        
        ## remove un-necessary columns from the data
        hits <- hits[, c(i+1, i+3)]
        colnames(hits) <- c("Pred", "Prob")
        #print(hits)
        
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
  
  if (exists("hits_df")) {
    hits_df <- dcast(melt(hits_df, id.vars = c("Pred")), Pred ~ ., sum)
    colnames(hits_df) <- c("Pred", "Prob")
    hits_df <- hits_df[order(hits_df$Prob, decreasing = T), ]
    
    ## Cleanup profanities if user doesn't want to see any bad words.
    if (filter_profanity == T) {
        clean_hits <- hits_df$Pred %in% bad_words
        hits_df <- hits_df[!clean_hits,]        
    }
    head(hits_df, num_preds)  
  }
  else {
    ## No matches. Return most frequent 1 grams.
    res<-tail(freqdf[[1]], num_preds)
    res$Prob <- res$Count/sum(freqdf[[1]]$Count)
    res <- res[,c("Term.1","Prob")]
    colnames(res) <- c("Pred", "Prob")
    res <- res[order(res$Prob, decreasing = T), ]
    res
  }
}
