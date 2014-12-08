predict_accuracy <- function(input_text, lines, grams=5, preds=5) {
    text <- input_text[1:lines]
    text_grams <- lapply(text, FUN = get_last_n_words, n=grams)
    predictions <- sapply(text_grams, simplify = "array", USE.NAMES=FALSE, FUN=get_prediction, num_preds=preds)
    pred_acc<- apply(predictions, 1, sum)/lines   
    pred_acc
}

get_last_n_words <- function(text, n=5) {
    words <- unlist(strsplit(text, " "))
    if (length(words) < n) {
        words
    }
    else {
        words[(length(words)-n+1):length(words)]
    }
    
}

get_prediction <- function(clean_words, num_preds) {
    len <- length(clean_words)
    print(paste("Getting Prediction for: "), quote=F)
    print(paste(clean_words[-len]))
    pred <- predict_next_word(clean_words[1:(len-1)], 0, num_preds)
    print("Predictions:", quote=F)
    print(paste(pred))
    print(paste("Actual Word: ", clean_words[len]))
    res <- c(clean_words[len] %in% head(pred,1), clean_words[len] %in% head(pred,2),
             clean_words[len] %in% head(pred,3), clean_words[len] %in% head(pred,4),
             clean_words[len] %in% head(pred,5))
    res
}