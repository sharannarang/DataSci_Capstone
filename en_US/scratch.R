## Capstone scratch script

## Exploratory Analysis

chars.blogs <- nchar(readLines("en_US.blogs.txt", encoding = 'UTF-8'))
chars.twitter <- nchar(readLines("en_US.twitter.txt", encoding = 'UTF-8'))
chars.news <- nchar(readLines("en_US.news.txt", encoding = 'UTF-8'))

par(mfrow = c(1,3))
boxplot(chars.blogs, col="green", log="y")
title(main="Blogs", ylab="Log scale of length of entries")
boxplot(chars.news, col="blue", log="y")
title(main="News", ylab="Log scale of length of entries")
boxplot(chars.twitter, col="red")
title(main="Tweets", ylab="Length of entries")

require(tm)

set.seed(123124)

## Pick random texts from the files
blogs <- create_sample("en_US.blogs.txt", "sample/blogs.txt", 899288)
news <- create_sample("en_US.news.txt", "sample/news.txt", 1010242)
tweets <- create_sample("en_US.twitter.txt", "sample/twitter.txt",2360148)

## Create the corpus
en.cor <- VCorpus(DirSource("sample/"))

## Perform the preprocessing
en.cor<- tm_map(en.cor, tolower)
en.cor<- tm_map(en.cor, removeNumbers)
en.cor<- tm_map(en.cor, removePunctuation)
en.cor <- tm_map(en.cor, removeWords, stopwords("english"))
en.cor <- tm_map(en.cor, stripWhitespace)
en.cor <- tm_map(en.cor, PlainTextDocument)

## Tokenization
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

## Create 1,2,3 gram tokens
dtm_uni <- DocumentTermMatrix(en.cor)
dtm_bi <- DocumentTermMatrix(en.cor, control = list(tokenize= BigramTokenizer))
dtm_tri <- DocumentTermMatrix(en.cor, control = list(tokenize= TrigramTokenizer))
dtm_quad <- DocumentTermMatrix(en.cor, control = list(tokenize= QuadgramTokenizer))
dtm_quad <- DocumentTermMatrix(en.cor, control = list(tokenize= PentagramTokenizer))


## Generate Frequencies
freq_uni <- colSums(as.matrix(dtm_uni))

## Create Sparse Matrices
dtm_uni_sp <- removeSparseTerms(dtm_uni,0.2)
dtm_bi_sp <- removeSparseTerms(dtm_bi,0.2)
dtm_tri_sp <- removeSparseTerms(dtm_tri,0.2)
dtm_quad_sp <- removeSparseTerms(dtm_quad,0.2)


## Popular terms for ngrams
popular_terms_uni <- generate_popular_terms_df(dtm_uni, 60)
popular_terms_bi <- generate_popular_terms_df(dtm_bi, 30)
popular_terms_tri <- generate_popular_terms_df(dtm_tri, 30)
popular_terms_quad <- generate_popular_terms_df(dtm_quad, 30)

## 1-grams without stop words
stop.words <- popular_terms_uni$n.grams %in% stopwords("en")
popular_terms_uni_ns <- popular_terms_uni[stop.words==FALSE,]



