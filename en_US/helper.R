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
    clean.text <- gsub("[^[:alpha:][:space:]'/-]", " ", clean.text)
    
    ## Convert to lowercase
    clean.text <- tolower(clean.text)
    
    ## Replace / with space since that's generally how it works.
    clean.text <- gsub("/", " ", clean.text)
    
    ## Remove quoted text
    clean.text <- gsub(" \'|\' ", " ", clean.text)
    clean.text <- gsub("^\'|\'$", "", clean.text)
    clean.text
    
    ## Replace it's with it is since it is very common
    clean.text <- gsub("it\'s", "it is", clean.text)
    clean.text <- gsub("that\'s", "that is", clean.text)
    clean.text
    
    ## Now, remove all 's
    clean.text <- gsub("\'s", "", clean.text)
    clean.text
    
    ## Remove ' - ' from data.
    clean.text <- gsub(" -+ | -+|-+ ", " ", clean.text)
    clean.text <- gsub("^-+|-+$", "", clean.text)

    
    ## Remove whitespace
    clean.text <- stripWhitespace(clean.text)
    clean.text <- gsub(" +$|^ +", "", clean.text)

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
