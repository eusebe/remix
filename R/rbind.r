rbind.list <- function(l) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
    results <- rbind(results, l[[i]])
  }
  results
}

cbind.list <- function(l) {
  n <- length(l)
  results <- NULL
  for (i in 1:n) {
    if (is.null(results))
      results <- l[[i]]
    else
      results <- cbind(results, l[[i]])
  }
  results
}
