buildNgramDatabase <- function() {
    library(tm)
    library(RWeka)
    library(RSQLite)
    
    #######
    ## Training stage
    #######
    
    # Load the text file
    conn <- file("data/samples/genesis.txt", "r")
    fulltext <- readLines(conn)
    close(conn)
    
    # Load the data into a corpus
    vs <- VectorSource(fulltext)
    text_corpus <- Corpus(vs, readerControl=list(readPlain, language="en",
                                                 load=TRUE))
    # Convert to lowercase
    text_corpus_proc <- tm_map(text_corpus, content_transformer(tolower))
    
    # Remove punctuation
    text_corpus_proc <- tm_map(text_corpus_proc, removePunctuation)
    
    # Remove extra whitespace
    text_corpus_proc <- tm_map(text_corpus_proc, stripWhitespace)
    
    # Create term-document matrix
    dtm <- DocumentTermMatrix(text_corpus_proc)
    dtm.matrix <- as.matrix(dtm)
    wordcount <- colSums(dtm.matrix)
    topten <- head(sort(wordcount, decreasing=TRUE), 10)
    
    options(mc.cores=1)
    twogramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=2, max=2))
    }
    
    dtm2 <- DocumentTermMatrix(text_corpus_proc,
                               control=list(tokenize=twogramTokenizer))
    dtm2.matrix <- as.matrix(dtm2)
    twogcount <- colSums(dtm2.matrix)
    
    threegramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=3, max=3))
    }
    
    dtm3 <- DocumentTermMatrix(text_corpus_proc,
                               control=list(tokenize=threegramTokenizer))
    dtm3.matrix <- as.matrix(dtm3)
    threegcount <- colSums(dtm3.matrix)
    
    fourgramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=4, max=4))
    }
    
    dtm4 <- DocumentTermMatrix(text_corpus_proc,
                               control=list(tokenize=fourgramTokenizer))
    dtm4.matrix <- as.matrix(dtm4)
    fourgcount <- colSums(dtm4.matrix)
    
    # Store the N-grams in a database
    driver <- dbDriver("SQLite")
    conn <- dbConnect(driver, dbname="ngrams.db")
    
    # Single words
    query <- paste0("DROP TABLE IF EXISTS onegrams")
    dbGetQuery(conn, query)
    
    df <- data.frame(
        word=names(wordcount),
        probability=unname(wordcount)/sum(wordcount)
    )
    dbWriteTable(conn, "onegrams", df)
    
    # Two-grams
    # 
    query <- paste0("DROP TABLE IF EXISTS twograms")
    dbGetQuery(conn, query)
    
    keyval <- strsplit(names(twogcount), " ")
    keys <- sapply(keyval, "[[", 1)
    values <- sapply(keyval, "[[", 2)
    df <- data.frame(
        ngram=names(twogcount),
        key=keys,
        value=values,
        probability=unname(twogcount)/sum(twogcount)
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
        probability=unname(threegcount)/sum(threegcount)
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
        probability=unname(fourgcount)/sum(fourgcount)
    )
    dbWriteTable(conn, "fourgrams", df)
    
    dbDisconnect(conn)
}