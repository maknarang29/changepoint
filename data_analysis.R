library(rjson);

load.dataset <- function(filename){
    data <- fromJSON(file = filename)

    # reformat the data into a data frame with a time index and the data values
    tidx <- data$time$index

    cols <- c()

    mat <- NULL
    for (j in 1:data$n_dim) {
        s <- data$series[[j]]
        v <- NULL
        for (i in 1:data$n_obs) {
            val <- s$raw[[i]]
            if (is.null(val)) {
                v <- c(v, NA)
            } else {
                v <- c(v, val)
            }
        }
        cols <- c(cols, s$label)
        mat <- cbind(mat, v)
    }

    mat <- cbind(tidx, mat)
    colnames(mat) <- c('t', cols)

    df <- as.data.frame(mat)
    return(df)
}


source("simulation_1.R")

data <- load("DBdata[asmt][v4.495].RData")
colnames(data)
length(ssb.data)
ssb.data$CODICE[!is.na(ssb.data$CODICE)]
rownames(ssb.data["CODICE"])
1
