add2 <- function(v1, v2) {
        return(v1 + v2) ##Return added values
}

above10 <- function(vec) {
        return(vec[(vec > 10) & (!is.na(vec))]) ##Return subset ot vec greater than 10
}

above <- function(vec, n = 10) {
        return(vec[(vec > n) & (!is.na(vec))]) ##Return subset ot vec greater than n
}

colmean <- function(df, removeNA = TRUE) {
        nc <- ncol(df)
        means <- numeric(nc)
        for(i in 1:nc) {
                col <- df[,i]
                if(removeNA) {
                        col <- col[!is.na(col)]
                }
                means[i] <- mean(col)
        }
        return(means)
}
