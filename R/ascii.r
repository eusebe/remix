ascii.remix <- function(x) {
  if (length(attr(x, "num")) == 0 & is.list(x)) {
    rn <- lapply(lapply(x, rownames), function(x) sub("([0-9a-zA-Z]+)( ?\\: ?)(.+)", "\\1", x))
    lgroup <- lapply(rn, function(x) c("", unique(x)))
    n.lgroup <- lapply(rn, function(x) c(1, table(x)))
    stats <- unique(lapply(lapply(x, rownames), function(x) sub("([0-9a-zA-Z]+)( ?\\: ?)(.+)", "\\3", x))[[1]])

    include.rownames <- FALSE
    nx <- names(x)
    
    res <- ascii(as.list(stats))
    for (z in 1:length(x)) {
      if (length(x[[z]]) == 0) next
      res <- asciiMixed$new(res, ascii(x[[z]], include.rownames = include.rownames, include.colnames = TRUE, caption = nx[z], lgroup = lgroup[[z]], n.lgroup = n.lgroup[[z]]))
      class(res) <- c("ascii", "proto", "environment")
    }
  } else if (is.list(x)) {
    nx <- names(x)
    res <- NULL
    
    for (z in 1:length(x)) {
      if (length(x[[z]]) == 0) next
      res <- asciiMixed$new(res, ascii(x[[z]], include.rownames = TRUE, include.colnames = TRUE, caption = nx[z]))
      class(res) <- c("ascii", "proto", "environment")
    }
  } else {
    res <- ascii(unclass(x), include.rownames = TRUE, include.colnames = TRUE)
  }
  res
}
