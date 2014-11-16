## Create the sample
create_sample <- function(src, out, lines) {
    text <- readLines(src, lines)
    sub<- rbinom(lines, 1, 0.2)
    out.txt <- text[sub==1]
    writeLines(out.txt, out)
    out.txt
}

## Cleaning function
clean_up <- function(text) {
    clean.text <- gsub("\u2092", "'", text)
    clean.text <- gsub("\u2019", "'", clean.text)
    clean.text <- removePunctuation(clean.text)
    clean.text <- removeNumbers(clean.text)
    clean.text <- removePunctuation(gsub("\032", "", clean.text))
    clean.text <- gsub("\u0093|\u0092|\u0094", "", clean.text)
    clean.text <- stripWhitespace(clean.text)
    clean.text <- tolower(clean.text)
    clean.text
}

plot_ngram_histogram <- function(freq, title){
    g <- ggplot(data = data.frame(n.grams=names(freq), count=log2(freq)), aes(x=count)) + geom_histogram(color="black", fill="darkblue2") + ggtitle(title)
    g
}

## Popular words
generate_popular_terms_df <- function(dtm, terms) {
    popular_terms <- colSums(as.matrix(dtm))
    popular_terms <- data.frame(n.grams=names(popular_terms), freq=popular_terms)
    head(popular_terms[order(popular_terms$freq, decreasing = T), ],terms)    
}

## Generate plot
plot_popular_terms <- function(wc, terms, fill_color="darkblue") {
    ggplot(data=head(wc, terms), aes(n.grams,freq)) + geom_bar(stat="identity", color="black", fill=fill_color) + 
        theme(axis.text.x=element_text(angle=45, hjust=1))    
}
