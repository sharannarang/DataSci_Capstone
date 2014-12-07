library(tm)

## Funciton to clean the input text.

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