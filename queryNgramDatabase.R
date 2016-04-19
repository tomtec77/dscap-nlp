queryNgramDatabase <- function(key, strlen, conn=conn, nwords=5) {
    
    if (strlen>=3) { dbname <- "fourgrams" }
    if (strlen==2) { dbname <- "threegrams" }
    if (strlen==1) { dbname <- "twograms" }
    
    my.query <- paste0("SELECT value, probability ",
                       "FROM ", dbname, " ",
                       "WHERE key='", key, "' ",
                       "ORDER BY probability DESC ",
                       "LIMIT ", nwords)
    res <- dbSendQuery(conn, my.query)
    df <- dbFetch(res)
    
    df$value
}