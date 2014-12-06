## Create the sample
create_sample <- function(src, out, lines) {
    text <- readLines(src, lines)
    sub<- rbinom(lines, 1, 0.2)
    out.txt <- text[sub==1]
    writeLines(out.txt, out)
    out.txt
}

## Cleaning function
## apostrophe s
## apostrophe it's, that's, she's, he's 
## hypen so-called -> so called . Keep hypen?

clean_up <- function(text) {
    ## Replace funny apostrophe's into simple ones
    clean.text <- gsub("\u2092", "'", text)
    clean.text <- gsub("\u2019", "'", clean.text)
    
    ## Replace funny characters
    clean.text <- gsub("\u0093|\u0092|\u0094", "", clean.text)    
    clean.text <- gsub("\032", "", clean.text)
    
    ## Remove anything that's not alphabetical or ' or / or -
    clean.text <- gsub("[^[:alpha:][:space:]']", " ", clean.text)
    
    ## Convert to lowercase
    clean.text <- tolower(clean.text)
    
    ## Replace / with space since that's generally how it works.
    clean.text <- gsub("/", " ", clean.text)
    
    ## Remove quoted text
    clean.text <- gsub(" \'+|\'+ ", " ", clean.text)
    clean.text <- gsub("^\'+|\'+$", "", clean.text)
    clean.text
    
    ## Replace it's with it is since it is very common
    clean.text <- gsub("it\'s", "it is", clean.text)
    clean.text <- gsub("that\'s", "that is", clean.text)
    clean.text
    
    ## Now, remove all 's
    clean.text <- gsub("\'s", "", clean.text)
    clean.text
    
    ## Remove ' - ' from data.
    ## clean.text <- gsub(" -+ | -+|-+ ", " ", clean.text)
    ## clean.text <- gsub("^-+|-+$", "", clean.text)

    
    ## Remove whitespace
    clean.text <- stripWhitespace(clean.text)
    clean.text <- gsub(" +$|^ +", "", clean.text)

    clean.text
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

get_3gram_count <- function(text) {
    search_string <- c(paste("^",text[1], "$", sep=""), paste("^", text[2], "$", sep=""), paste("^", text[3], "$", sep=""))
    gram <- tail(freqdf_tri[grepl(search_string[1], freqdf_tri$Term.1) & grepl(search_string[2], freqdf_tri$Term.2) & grepl(search_string[3], freqdf_tri$Term.3), ],1)
    gram$Count
}

get_4gram_count <- function(text) {
  search_string <- c(paste("^",text[1], "$", sep=""), paste("^", text[2], "$", sep=""), paste("^", text[3], "$", sep=""), paste("^", text[4], "$", sep=""))
  gram <- tail(freqdf_quad[grepl(search_string[1], freqdf_quad$Term.1) & grepl(search_string[2], freqdf_quad$Term.2) & grepl(search_string[3], freqdf_quad$Term.3) & grepl(search_string[4], freqdf_quad$Term.4), ],1)
  gram$Count
}

search_3gram_freq_df <- function(text) {
    search_string <- c(paste("^",text[1], "$", sep=""), paste("^", text[2], "$", sep=""))
    tail(freqdf_tri[grepl(search_string[1], freqdf_tri$Term.1) & grepl(search_string[2], freqdf_tri$Term.2), ],5)
}

search_4gram_freq_df <- function(text) {
    search_string <- c(paste("^",text[1], "$", sep=""), paste("^", text[2], "$", sep=""), paste("^", text[3], "$", sep=""))
    tail(freqdf[[4]][grepl(search_string[1], freqdf_quad$Term.1) & grepl(search_string[2], freqdf_quad$Term.2) & grepl(search_string[3], freqdf_quad$Term.3), ],5)
}

search_5gram_freq_df <- function(text) {
    search_string <- c(paste("^",text[1], "$", sep=""), 
                       paste("^", text[2], "$", sep=""), 
                       paste("^", text[3], "$", sep=""),
                       paste("^", text[4], "$", sep="")                       
                       )
    tail(freqdf_penta[grepl(search_string[1], freqdf_penta$Term.1) & 
                      grepl(search_string[2], freqdf_penta$Term.2) & 
                      grepl(search_string[3], freqdf_penta$Term.3) &
                      grepl(search_string[4], freqdf_penta$Term.4)   , ],5)
}

split_search_string <- function(text, len) {
    search_string <- paste("^",text[1], "$", sep="")
    if (len != 1) {
        for (i in seq(2, len)) {
            search_string <- c(search_string, paste("^",text[i], "$", sep=""))  
        }  
    }
    search_string
}

get_predictions <- function(words, len) {
    res <- search_freq_df(words, len, len+1, 5)
    print(res)
    res
}

get_count <- function(words, len) {
    res <- search_freq_df(words, len, len, 1)
    res$Count
}

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

search_terms <- function(text_string) {
    clean_text <- clean_up(text_string)
    clean_words <- unlist(strsplit(clean_text, " "))
    len <- length(clean_words)
    
    ## Still need to handle the case where the input doesn't have any text.
    mult <- 0    
    for (i in seq(4,1)) {
        if (len >= i) {
            search_words <- split_search_string(tail(clean_words,i), i)
            hits <- get_predictions(search_words, i)
            if (nrow(hits)) {
                low_gram_count <- get_count(search_words, i)
                prob_mult <- ifelse(mult==0, 1, (0.4**mult))
                hits$Prob <- prob_mult * hits$Count/low_gram_count 
                hits <- hits[, c(i+1, i+3)]
                colnames(hits) <- c("Pred", "Prob")
                print(hits)
                print(exists("hits_df"))
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
    hits_df <- dcast(melt(hits_df, id.vars = c("Pred")), Pred ~ ., sum)
    colnames(hits_df) <- c("Pred", "Prob")
    hits_df <- hits_df[order(hits_df$Prob, decreasing = T), ]
    head(hits_df,5)

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
