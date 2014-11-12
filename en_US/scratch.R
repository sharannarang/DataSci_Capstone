## Capstone scratch script

require(tm)

set.seed(123124)

blogs.lines <- 899288
con1 <- file("en_US.blogs.txt", 'r')
con2 <- file("blogs.txt", "w")

blogs <- readLines(con1, 1)
for (i in 2:blogs.lines) {
    if (rbinom(1,1,0.2)) {
        blogs <- c(blogs,readLines(con1,1))
    }
    else {
        gbg <- readLines(con1, 1)
    }
}
close(con1)

writeLines(blogs,con2)
close(con2)

blog.cor <- VCorpus(VectorSource(blogs))
blog.cor <- tm_map(blog.cor, stripWhitespace)
blog.cor <- tm_map(blog.cor, tolower)
