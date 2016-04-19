# Create the training dataset for a text prediction model, based on text
# collected from blogs, news feeds and twitter
# This script creates four databases:
#    - onegrams: list of individual word ocurrences
#    - twograms: list of two-grams with their frequencies
#    - threegrams: list of three-grams with their frequencies
#    - fourgrams: list of four-grams with their frequencies
# The databases are saved to a SQLite database file called 'ngrams.db'

#######
# Configuration
#######

# Set the RNG seed for reproducibility
rngseed     <- 83474

# The source data files are rather large - select a fraction of them to
# create a manageable data sample
fraction    <- 0.001

# Select which of the three data sources to use
use.twitter <- FALSE
use.news    <- TRUE
use.blogs   <- TRUE

# For text processing we use the tm package. Source data are collected in
# a structure called a corpus, and in principle, each line in the sample
# file is a document in the corpus. Having many documents results in the
# corpus taking up a lot of memory - a way to reduce this is to group
# several documents together. However, we don't want to form n-grams with
# words from different documents; to avoid this, we define an easily
# identifiable separator, and we'll discard any n-grams that contain it
group       <- 20
split       <- " 123break321 "

#######

library(tm)
library(RWeka)
library(RSQLite)

source("createCorpus.R")
source("textFileSample.R")

set.seed(rngseed)

# Create output directory if it doesn't exist
data_dir <- "data/"
training_dir = paste0(data_dir, "train/")
if (!(dir.exists(training_dir))) {
    dir.create(training_dir)
}

# Load blacklists
blacklist_dir <- "data/blacklists/"
word.blacklist <- read.table(
    paste0(blacklist_dir, "blacklist_oneword.txt"),
    stringsAsFactors=FALSE)$V1
twogram.blacklist <- read.table(
    paste0(blacklist_dir, "blacklist_twograms.txt"), sep=",",
    stringsAsFactors=FALSE)$V1

# Add the separator string to the blacklist
word.blacklist <- c(gsub("\\s+", "", split), word.blacklist)

# Collect samples of text from the source files
if (use.blogs) {
    cat("Extracting sample from blogs file...\n")
    block.start <- proc.time()

    blog_file <- paste0(training_dir, "blog_train.txt")
    if (!file.exists(blog_file)) {
        textFileSample(paste0(data_dir, "final/en_US/en_US.blogs.txt"),
                       blog_file, fraction=fraction, group=group,
                       split=split)
    }
    train_corpus <- createCorpus(blog_file)

    cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"],
        "\n\n")
}

if (use.news) {
    cat("Extracting sample from news file...\n")
    block.start <- proc.time()

    news_file <- paste0(training_dir, "news_train.txt")
    if (!file.exists(news_file)) {
        textFileSample(paste0(data_dir, "final/en_US/en_US.news.txt"),
                       news_file, fraction=fraction, group=group,
                       split=split)
    }

    if (use.blogs) {
        train_corpus <- c(train_corpus, createCorpus(news_file))
    } else {
        train_corpus <- createCorpus(news_file)
    }

    cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"],
        "\n\n")
}

if (use.twitter) {
    cat("Extracting sample from twitter file...\n")
    block.start <- proc.time()

    twit_file <- paste0(training_dir, "twit_train.txt")
    if (!file.exists(twit_file)) {
        textFileSample(paste0(data_dir, "final/en_US/en_US.twitter.txt"),
                       twit_file, fraction=fraction, group=group,
                       split=split)
    }

    if (use.blogs | use.news) {
        train_corpus <- c(train_corpus, createCorpus(twit_file))
    } else {
        train_corpus <- createCorpus(twit_file)
    }

    cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"],
        "\n\n")
}


# Text preprocessing
cat("Preprocessing documents...\n")
block.start <- proc.time()

# Convert all the text to lowercase
train_corpus <- tm_map(train_corpus, content_transformer(tolower))

# Remove punctuation
train_corpus <- tm_map(train_corpus, removePunctuation)

# Strip whitespace
train_corpus <- tm_map(train_corpus, stripWhitespace)

cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"], "\n\n")

# Create term-document matrix and count single word ocurrences
# We discard from the results the document break marker and all words in
# the blacklist
cat("Generating term-document matrix for single words...\n")
block.start <- proc.time()

dtm <- DocumentTermMatrix(train_corpus)
dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)

wordcount <- wordcount[!(names(wordcount) %in% c(gsub("\\s+", "", split),
                                                 word.blacklist))]
totwords <- sum(wordcount)
wordcount <- wordcount/totwords

# Cleanup - remove unnecessary objects
rm(list=c("dtm","dtm.matrix"))

cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"], "\n\n")

# Create term-document matrix for two-grams
# In this case, we'll drop any two-gram which either contains a
# blacklisted word, or if it's a combination of two non-blacklisted words
# that form an offensive pair
cat("Generating term-document matrix for two-grams...\n")
block.start <- proc.time()

options(mc.cores=1)
twogramTokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min=2, max=2))
}

dtm2 <- DocumentTermMatrix(train_corpus,
                           control=list(tokenize=twogramTokenizer))
# Warning! This operation is a memory hog
dtm2.matrix <- as.matrix(dtm2)
twogcount <- colSums(dtm2.matrix)

# Remove blacklisted two-grams
findBlacklistedEntries <- function(wordlist, blacklist) {
    sapply(wordlist, function(x) {
        sum(sapply(blacklist, function(y) { grepl(y, x) }))>0
    })
}
blacklist.words <- findBlacklistedEntries(names(twogcount),
                                                c(word.blacklist,
                                                  gsub("\\s+","",split)))
blacklist.twograms <- findBlacklistedEntries(names(twogcount),
                                             twogram.blacklist)
blacklist <- blacklist.words & blacklist.twograms
twogcount <- twogcount[!blacklist]

tottwograms <- sum(twogcount)
twogcount <- twogcount/tottwograms

# Cleanup - remove unnecessary objects
rm(list=c("dtm2","dtm2.matrix"))

cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"], "\n\n")

# Create term-document matrix for three-grams
cat("Generating term-document matrix for three-grams...\n")
block.start <- proc.time()

threegramTokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min=3, max=3))
}

dtm3 <- DocumentTermMatrix(train_corpus,
                           control=list(tokenize=threegramTokenizer))
dtm3.matrix <- as.matrix(dtm3)
threegcount <- colSums(dtm3.matrix)

# Remove blacklisted three-grams
blacklist.words <- findBlacklistedEntries(names(threegcount),
                                                c(word.blacklist,
                                                  gsub("\\s+","",split)))
blacklist.twograms <- findBlacklistedEntries(names(threegcount),
                                             twogram.blacklist)
blacklist <- blacklist.words & blacklist.twograms
threegcount <- threegcount[!blacklist]

totthreegrams <- sum(threegcount)
threegcount <- threegcount/totthreegrams

# Cleanup - remove unnecessary objects
rm(list=c("dtm3","dtm3.matrix"))

cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"], "\n\n")

# Create term-document matrix for four-grams
cat("Generating term-document matrix for four-grams...\n")
block.start <- proc.time()

fourgramTokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min=4, max=4))
}

dtm4 <- DocumentTermMatrix(train_corpus,
                           control=list(tokenize=fourgramTokenizer))
dtm4.matrix <- as.matrix(dtm4)
fourgcount <- colSums(dtm4.matrix)

# Remove blacklisted four-grams
blacklist.words <- findBlacklistedEntries(names(fourgcount),
                                                c(word.blacklist,
                                                  gsub("\\s+","",split)))
blacklist.twograms <- findBlacklistedEntries(names(fourgcount),
                                             twogram.blacklist)
blacklist <- blacklist.words & blacklist.twograms
fourgcount <- fourgcount[!blacklist]

totfourgrams <- sum(fourgcount)
fourgcount <- fourgcount/totfourgrams

# Cleanup - remove unnecessary objects
rm(list=c("dtm4","dtm4.matrix", "blacklist.words", "blacklist.twograms",
          "blacklist"))

cat("Time taken:", proc.time()["elapsed"]-block.start["elapsed"], "\n\n")

# Store all the N-gram frequencies in a database, but splitting them
# into the last word ('value') and all the others ('key')
driver <- dbDriver("SQLite")
conn <- dbConnect(driver, dbname="ngrams.db")

# Single words
query <- paste0("DROP TABLE IF EXISTS onegrams")
dbGetQuery(conn, query)

df <- data.frame(
    word=names(wordcount),
    probability=unname(wordcount)
)
dbWriteTable(conn, "onegrams", df)

# Two-grams
query <- paste0("DROP TABLE IF EXISTS twograms")
dbGetQuery(conn, query)

keyval <- strsplit(names(twogcount), " ")
keys <- sapply(keyval, "[[", 1)
values <- sapply(keyval, "[[", 2)
df <- data.frame(
    ngram=names(twogcount),
    key=keys,
    value=values,
    probability=unname(twogcount)
)
dbWriteTable(conn, "twograms", df)

# Three-grams
query <- paste0("DROP TABLE IF EXISTS threegrams")
dbGetQuery(conn, query)

keyval <- strsplit(names(threegcount), " ")
keys <- paste(sapply(keyval, "[[", 1), sapply(keyval, "[[", 2))
values <- sapply(keyval, "[[", 3)
df <- data.frame(
    ngram=names(threegcount),
    key=keys,
    value=values,
    probability=unname(threegcount)
)
dbWriteTable(conn, "threegrams", df)

# Four-grams
query <- paste0("DROP TABLE IF EXISTS fourgrams")
dbGetQuery(conn, query)

keyval <- strsplit(names(fourgcount), " ")
keys <- paste(sapply(keyval, "[[", 1), sapply(keyval, "[[", 2),
              sapply(keyval, "[[", 3))
values <- sapply(keyval, "[[", 4)
df <- data.frame(
    ngram=names(fourgcount),
    key=keys,
    value=values,
    probability=unname(fourgcount)
)
dbWriteTable(conn, "fourgrams", df)

dbDisconnect(conn)

cat("Training dataset complete.\n\n")
