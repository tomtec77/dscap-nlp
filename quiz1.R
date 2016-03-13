# Coursera Data Science Capstone Project - Quiz 1

# Question 1: get size of file en_US.blogs.txt
size <- file.info("data/final/en_US/en_US.blogs.txt")$size/1048576

# Question 2: get the number of lines in file en_US.twitter.txt
nlines <- length(readLines("data/final/en_US/en_US.twitter.txt"))

# Question 3: what is the length of the longest line in any of the 3 en_US
# files
# We won't count in the Twitter file as those have a restriction of 140
# characters, so the longest line is almost certainly in one of the other
# two files
findLongestLine <- function(filepath) {
  fulltext = readLines(filepath)
  
  maxline <- 0
  
  for (i in 1:length(fulltext)) {
    linelength <- nchar(fulltext[i])
    if (linelength > maxline) { maxline <- linelength }
  }
  maxline
}

maxline_blogs <- findLongestLine("data/final/en_US/en_US.blogs.txt")
maxline_news <- findLongestLine("data/final/en_US/en_US.news.txt")

# Question 4: in the en_US.twitter.txt file, find the number of lines that
# contain "love" (all lowercase) and "hate" and get the ratio of those two
# numbers
countLinesWithText <- function(filepath, textstring) {
  fulltext = readLines(filepath)
  sum(grep(textstring, fulltext)>0)
}

filepath <- "data/final/en_US/en_US.twitter.txt"
nlove <- countLinesWithText(filepath, "love")
nhate <- countLinesWithText(filepath, "hate")

print(nlove/nhate)

# Question 5: find the one tweet in the en_US dataset that contains the
# word 'biostats'
fulltext <- readLines(filepath)
fulltext[grep("biostats", fulltext)]

# Question 6: count how many tweets have the exact phrase "A computer once
# beat me at chess, but it was no match for me at kickboxing"
phrase <- "A computer once beat me at chess, but it was no match for me at kickboxing"
sum(grep(phrase, fulltext)>0)