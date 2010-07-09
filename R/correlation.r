##' Compute correlation
##'
##' @param x numerical object
##' @param y numerical object
##' @param method method
##' @author David Hajage
##' @keywords internal
correlation <- function(x, y, method = c("pearson", "kendall", "spearman")) {
  results <- cor.test(x, y, method = method)$estimate
  class(results) <- c("correlation", "matrix")
  results
}

##' Compute correlation (data.frame input)
##'
##' @param dfx data.frame
##' @param dfy data.frame
##' @param method method
##' @author David Hajage
##' @keywords internal
correlation.data.frame <- function(dfx, dfy, method = c("pearson", "kendall", "spearman")) {
  results <- sapply(dfy, function(y) sapply(dfx, correlation, y, method = method))
  if (ncol(dfx) == 1) {
    dim(results) <- c(1, ncol(dfy))
  }
  rownames(results) <- names(dfx)
  colnames(results) <- names(dfy)
  class(results) <- c("correlation", "matrix")

  attr(results, "dfx") <- dfx
  attr(results, "dfy") <- dfy
  
  results
}

plot.correlation <- function(x, ...) {
  dfx <- attr(x, "dfx")
  dfy <- attr(x, "dfy")

  xy <- unlist(lapply(dfx, function(x) lapply(dfy, function(y) cbind(x, y))), recursive = FALSE)
  cnames <- rev(expand.grid(names(dfy), names(dfx), stringsAsFactors = FALSE))
  xyp <- NULL
  for (i in 1:length(xy)) {
    df <- data.frame(xy[[i]])
    xyp <- c(xyp, list(ggplot(df, aes(x, y)) +
             geom_point() +
             stat_smooth(method = lm, se = FALSE) +
             theme_bw() +
             xlab(cnames[i, 1]) +
             ylab(cnames[i, 2])))
  }
  
  do.call(arrange, c(xyp, nrow = ncol(dfx), ncol = ncol(dfy)))
}

##' Ascii for correlation object.
##'
##' Ascii method for correlation object (internal).
##'
##' @export
##' @param x a correlation object
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param include.rownames see \code{?ascii} in \code{ascii} package
##' @param include.colnames see \code{?ascii} in \code{ascii} package
##' @param header see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords internal
ascii.correlation <- function(x, format = "nice", digits = 5, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, ...) {
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
}

##' Print correlation object.
##'
##' Print correlation object (internal).
##'
##' @export
##' @param x a correlation object
##' @param type type of output (see \code{?ascii} in \code{ascii}
##' package)
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords internal
print.correlation <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

##' as.data.frame for correlation object.
##'
##' as.data.frame for correlation object (internal).
##'
##' @export
##' @param x a correlation object
##' @param ... not used
##' @author David Hajage
##' @keywords internal
as.data.frame.correlation <- function(x, ...) {
  as.data.frame(unclass(x))
}

##' Test if \code{x} is a correlation object
##'
##' @param x a correlation object
##' @author David Hajage
##' @keywords internal
is.correlation <- function(x)
  inherits(x, "correlation")
