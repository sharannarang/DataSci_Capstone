## Create the sample
create_sample <- function(src, out, lines) {
    text <- readLines(src, lines)
    sub<- rbinom(lines, 1, 0.2)
    out.txt <- text[sub==1]
    writeLines(out.txt, out)
    out.txt
}

plot_ngram_histogram <- function(freq, title){
    g <- ggplot(data = data.frame(n.grams=names(freq), count=log2(freq)), aes(x=count)) + geom_histogram(color="black", fill="darkblue") + ggtitle(title)
    g
}

## Popular words
generate_popular_terms_df <- function(dtm, terms) {
    popular_terms <- colSums(as.matrix(dtm))
    popular_terms <- data.frame(n.grams=names(popular_terms), freq=popular_terms)
    head(popular_terms[order(popular_terms$freq, decreasing = T), ],terms)    
}

## Generate plot
plot_popular_terms <- function(wc, terms, title, fill_color="darkblue") {
    ggplot(data=head(wc, terms), aes(n.grams,freq)) + geom_bar(stat="identity", color="black", fill=fill_color) + 
        theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle(title)
}

create_freq_table <- function(dtm) {
    freq <- colSums(as.matrix(dtm))
    freq <- freq[order(freq)]
    freq
}

create_words_data_frame <- function(counts) {
     words.list <- strsplit(names(counts), " ")
     df <- data.frame(matrix(unlist(words.list), nrow = length(counts), byrow = T))
     for (i in 1:ncol(df)) {
         colnames(df)[i] <- paste("Term", i, sep=".")
         df[,i] <- as.character(df[,i])
     }     
     df <- cbind(df, counts, row.names=NULL)
     colnames(df)[ncol(df)] <- "Count"
     df

}

#     if (len >= 1) { ## Atleast 1 words in input
#        ##Implement 2 gram search
#     }
#     if (len >= 2) {
#         #search_3gram_freq_df(tail(clean_words,2))
#     }
#     if (len >= 3) {
#         search_words <- split_search_string(tail(clean_words,3), 3)
#         quad_hits <- get_predictions(search_words, 3)
#         if (nrow(quad_hits)) {
#             tri_count <- get_count(search_words, 3)
#             quad_hits$Prob <- 0.4 * (quad_hits$Count/tri_count)
#             quad_hits <- quad_hits[,c(4,6)]
#             colnames(quad_hits) <- c("Pred", "Prob")
#         }
# #         quad_hits <- search_4gram_freq_df(tail(clean_words,3))
# #         if (nrow(quad_hits)) {
# #             tri_count <- get_3gram_count(tail(clean_words,3))          
# #             quad_hits$Prob <- 0.4 * (quad_hits$Count/tri_count)
# #             quad_hits <- quad_hits[,c(4,6)]
# #             colnames(quad_hits) <- c("Pred", "Prob")
# #         }
#     }
#     if (len >= 4) {
#       penta_hits <- search_5gram_freq_df(tail(clean_words,4))
#       if (nrow(penta_hits)) {
#         quad_count <- get_4gram_count(tail(clean_words,4))          
#         penta_hits$Prob <- penta_hits$Count/quad_count
#         penta_hits <- penta_hits[,c(5,7)]
#         colnames(penta_hits) <- c("Pred", "Prob")
#       }       
#     }
#     if (len >= 4)
#        rbind(quad_hits,penta_hits, row.names=NULL)
#     else
#       quad_hits
