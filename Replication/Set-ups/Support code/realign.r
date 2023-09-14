realign <- function(X) {
  # Realign all individual time series of predictor
  # matrix X so that they artificially end at the same time.
  
  length <- dim(X)[1]
  width <- dim(X)[2]
  max_NA <- NULL
  for (a in 1:width) {
    nr_NA <- sum(is.na(X[,a]))
    if (nr_NA != 0){
      X[,a] <- c(rep(NA,nr_NA),X[,a][-((length-nr_NA+1):length)])
      max_NA <- max(max_NA, nr_NA)
          }
  }
  
  output <- list(X, max_NA)
  return(output)
}

