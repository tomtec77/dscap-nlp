library(tm)

# Load the first 10 lines of the news data file
conn <- file("data/samples/news_sample.txt", "r")
fulltext <- readLines(conn, 10)
close(conn)

# Load the data into a corpus
vs <- VectorSource(fulltext)
news_corpus <- Corpus(vs, readerControl=list(readPlain, language="en",
                                             load=TRUE))

# Preprocessing tasks
# Convert all to lowercase
news_corpus_lc <- tm_map(news_corpus, tolower)

# Remove stopwords
news_corpus_sw <- tm_map(news_corpus_lc, removeWords,
                         stopwords(kind="en"))

# Remove extra whitespace
news_corpus_ws <- tm_map(news_corpus_sw, stripWhitespace)

# Remove punctuation
news_corpus_pn <- tm_map(news_corpus_ws, removePunctuation)

# Remove numbers
news_corpus_nn <- tm_map(news_corpus_pn, removeNumbers)
