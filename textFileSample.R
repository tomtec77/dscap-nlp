textFileSample <- function(infile, outfile, split=".", fraction=0.5, group=1) {
    conn <- file(infile, "r")
    fulltext <- readLines(conn)
    nlines <- length(fulltext)
    close(conn)
    
    conn <- file(outfile, "w")
    selection <- rbinom(nlines, 1, fraction)
    counter <- 1
    for (i in 1:nlines) {
        if (selection[i]==1) {
            cat(fulltext[i], file=conn)
            if ((counter %% group)==0) {
                cat("\n", file=conn)
                counter <- 1
            } else {
                cat(split, file=conn)
                counter <- counter + 1
            }
        }
    }
    close(conn)
    
    paste("Saved", sum(selection), "lines to file", outfile)
}