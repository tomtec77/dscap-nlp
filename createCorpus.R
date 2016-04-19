createCorpus <- function(filepath, language="en", load=TRUE) {
    require(tm)
    
    conn <- file(filepath, "r")
    fulltext <- readLines(conn)
    close(conn)
    
    vs <- VectorSource(fulltext)
    Corpus(vs, readerControl=list(readPlain, language=language, load=load))
}
