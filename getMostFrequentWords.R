getMostFrequentWords <- function(nwords, conn=conn) {
    my.query <- paste0("SELECT word, probability ",
                       "FROM onegrams ",
                       "ORDER BY probability DESC ",
                       "LIMIT ", nwords)
    
    res <- dbSendQuery(conn, my.query)
    df <- dbFetch(res)
    
    df$word
}