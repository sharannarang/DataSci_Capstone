## Capstone scratch script

require(tm)

set.seed(123124)

create_sample <- function (input_file, output_file, lines) {
  con1 <- file(input_file, 'r')
  con2 <- file(output_file, "w")
  sample <- readLines(con1, 1)
  for (i in 2:lines) {
    if (rbinom(1,1,0.2)) {
      sample <- c(sample,readLines(con1,1))
    }
    else {
      gbg <- readLines(con1, 1)
    }
  }
  close(con1)
  
  writeLines(sample,con2)
  close(con2)    
  sample
}

blogs <- create_sample("en_US.blogs.txt", "sample/blogs.txt", 899288)
news <- create_sample("en_US.news.txt", "sample/news.txt", 1010242)
tweets <- create_sample("en_US.twitter.txt", "sample/twitter.txt",2360148)

en.cor <- VCorpus(DirSource("sample/"))

blog.cor <- VCorpus(VectorSource(blogs))
blog.cor <- tm_map(blog.cor, stripWhitespace)
blog.cor <- tm_map(blog.cor, tolower)
