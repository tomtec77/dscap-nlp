# Text predictor function
# Enter a string, take the last three words from it and predict the next
# word in the sequence
predictText <- function(text, conn=NULL, nwords=5) {
    
    # Remove punctuation from the input and convert to lowercase
    text <- gsub("[[:punct:]]", "", tolower(text))
    
    # Remove extra whitespace
    text <- gsub("\\s+", " ", text)
    
    # Get the key string: the last 3 words, or all of the input if it's
    # three words or less
    # Also calculate the (n-1)grams down to a single word in case we don't
    # find the key and have to try again
    key <- text
    keym1 <- NULL
    keym2 <- NULL
    strlen <- length(strsplit(text, " ")[[1]])
    
    if (strlen>1) {
        # Two words or more, extract the last word
        pattern <- "[^ ]*$"
        keym1 <- regmatches(text, regexpr(pattern, text))
    }
    if (strlen>2) {
        # Three words or more, extract the two-gram
        pattern <- "[^ ]* [^ ]*$"
        keym2 <- keym1
        keym1 <- regmatches(text, regexpr(pattern, text))
    } 
    if (strlen>3) {
        # Four words or more, keep only the final three-gram
        pattern <- "[^ ]* [^ ]* [^ ]*$"
        key <- regmatches(text, regexpr(pattern, text))
    }
    
    # To predict the next word, search for the key in the databases and
    # return up to nwords words with the highest probability
    wordlist <- queryNgramDatabase(key, strlen, conn=conn, nwords=nwords)
    
    # If we couldn't find any words, use stupid backoff to provide
    # alternatives starting from the (n-1)gram
    if (length(wordlist)==0) {
        if (!is.null(keym1)) {
            # Note that if keym1 is NULL then the same should be for keym2
            wordlist <- queryNgramDatabase(keym1, 2, conn=conn,
                                           nwords=nwords)
            if (length(wordlist)==0) {
                if (!is.null(keym2)) {
                    wordlist <- queryNgramDatabase(keym2, 1, conn=conn,
                                                   nwords=nwords)
                } else {
                    wordlist <- getMostFrequentWords(nwords, conn=conn)
                }
            }
        } else {
            wordlist <- getMostFrequentWords(nwords, conn=conn)
        }
    }
    
    wordlist
}