##' Concatenate functions
##'
##' @param ... functions
##' @author David Hajage
##' @keywords internal
funs2fun <- function(...) {
  fnames <- as.character(match.call()[-1])
  fs <- list(...)
  n <- length(fs)
  function(x, ...) {
    results <- NULL
    for (i in 1:n) {
      tmp <- match.fun(fs[[i]])(x, ...)
      names(tmp) <- paste(fnames[i], names(tmp))
      results <- c(results, tmp)
    }
    results
  }
}

##' Return the number of non NA observations
##'
##' @export
##' @param x a vector
##' @param na.rm not used
##' @author David Hajage
##' @keywords univar
n <- function(x, na.rm = FALSE) {
  sum(!is.na(x))
}

##' Return the number of NA observations
##'
##' @export
##' @param x a vector
##' @param na.rm not used
##' @author David Hajage
##' @keywords univar
na <- function(x, na.rm = FALSE) {
  sum(is.na(x))
}

##' Compute summary statistics
##'
##' @param x numeric object
##' @param funs functions
##' @param ... passed to funs
##' @author David Hajage
##' @keywords internal
summarize <- function(x, funs = c(mean, sd, quantile, n, na), ...) {
  if (!is.numeric(x))
    stop("x doit etre numerique")  

  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  if (length(funs) > 1)
    fun <- do.call(funs2fun, as.list(funs))
  else
    fun <- match.fun(funs)

  results <- fun(x, ...)
  class(results) <- c("summarize", "matrix")
  results
}

##' Compute summary statistics (data.frame input)
##'
##' @param df a data.frame
##' @param funs functions
##' @param ... passed to funs
##' @author David Hajage
##' @keywords internal
summarize.data.frame <- function(df, funs = c(mean, sd, quantile, n, na), ...) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  dfl <- as.list(df)
  results <- t(sapply(dfl, summarize, funs = funs, ...))  
  class(results) <- c("summarize", "matrix")
  if (length(funs) == 1) {
    if (length(match.fun(funs)(1:10, ...)) == 1) {
      dim(results) <- rev(dim(results))
      rownames(results) <- names(dfl)
      colnames(results) <- funs
    }
  }
  results
}

##' Ascii for summarize object.
##'
##' Ascii method for summarize object (internal).
##'
##' @export
##' @param x a summarize object
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param include.rownames see \code{?ascii} in \code{ascii} package
##' @param include.colnames see \code{?ascii} in \code{ascii} package
##' @param header see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
ascii.summarize <- function(x, format = "nice", digits = 5, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, ...) {
  if (is.null(nrow(x))) {
    x <- t(x)
  }
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
}

##' Print summarize object.
##'
##' Print summarize object (internal).
##'
##' @export
##' @param x a summarize object
##' @param type type of output (see \code{?ascii} in \code{ascii}
##' package)
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords internal
print.summarize <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

##' Test if \code{x} is a summarize object
##'
##' @param x a summarize object
##' @author David Hajage
##' @keywords internal
is.summarize <- function(x)
  inherits(x, "summarize")

##' Compute summary statistics according to a factor
##'
##' @param x numeric object
##' @param by factor
##' @param funs functions
##' @param ... passed to funs
##' @param useNA useNA
##' @author David Hajage
##' @keywords internal
summarize.by <- function(x, by, funs = c(mean, sd, quantile, n, na), ..., useNA = c("no", "ifany", "always")) {
  if (!is.numeric(x))
    stop("x doit etre numerique")  

  if (useNA[1] != "no") {
    by <- addNA(by)
  }
  
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  fun <- function(df, funs = funs, ...) summarize(df[, 1], funs = funs, ...)
  df <- data.frame(x, by, check.names = FALSE)
  names(df) <- c(names(df)[1], "by")
  results <- list(dlply(df, .(by), fun, funs = funs, ...))

  lgroup <- lapply(results, function(x) {
    y <- names(x)
    y[is.na(y)] <- "NA"
    y
  })
  n.lgroup <- NULL

  class(results) <- c("summarize.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  
  results
}

##' Compute summary statistics according to a factor (data.frame input)
##'
##' @param df data.frame
##' @param by data.frame
##' @param funs fuctions
##' @param ... passed to funs
##' @param useNA useNA
##' @author David Hajage
##' @keywords internal
summarize.data.frame.by <- function(df, by, funs = c(mean, sd, quantile, n, na), ..., useNA = c("no", "ifany", "always")) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  if (useNA[1] != "no") {
    for (i in 1:ncol(by))
      by[, i] <- addNA(by[, i])
  }

  nby <- ncol(by)
  namesby <- paste("by", 1:nby, sep = ".")
  fun <- function(df, funs = funs, ...) summarize.data.frame(df[, 1:(ncol(df)-nby), drop = FALSE], funs = funs, ...)
  df <- data.frame(df, by, check.names = FALSE)
  names(df) <- c(names(df)[1:(length(names(df))-nby)], namesby)
  results <- NULL
  for (i in 1:nby) {
    dfi <- df
    if (useNA[1] == "no") {
      dfi <- dfi[!is.na(by[, i]), ]
    }
    results <- c(results, list(dlply(dfi, namesby[i], fun, funs = funs, ...)))
  }
  names(results) <- colnames(by)

  r <- lapply(results, function(x) {
    y <- names(x)
    y[is.na(y)] <- "NA"
    y
  })
  
  lgroup <- list(unlist(r), names(results))
  nr <- nrow(results[[1]][[1]])
  n.lgroup <- list(nr, nr*sapply(r, length))

  class(results) <- c("summarize.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  
  results
}

##' Ascii for summarize.by object.
##'
##' Ascii method for summarize.by object (internal).
##'
##' @export
##' @param x a summarize.by object
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param include.rownames see \code{?ascii} in \code{ascii} package
##' @param include.colnames see \code{?ascii} in \code{ascii} package
##' @param header see \code{?ascii} in \code{ascii} package
##' @param lgroup see \code{?ascii} in \code{ascii} package
##' @param n.lgroup see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
ascii.summarize.by <- function(x, format = "nice", digits = 5, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), ...) {
  xx <- NULL
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])) {
      xx <- rbind(xx, x[[i]][[j]])
    }
  }

  class(x) <- class(x)[-1]
  ascii:::ascii(xx, lgroup = lgroup, n.lgroup = n.lgroup, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
}

##' Print summarize.by object.
##'
##' Print summarize.by object (internal).
##'
##' @export
##' @param x a summarize.by object
##' @param type type of output (see \code{?ascii} in \code{ascii} package)
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords internal
print.summarize.by <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

##' Test if \code{x} is a summarize.by object
##'
##' @param x a summarize.by object
##' @author David Hajage
##' @keywords internal
is.summarize.by <- function(x)
  inherits(x, "summarize.by")

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
  results
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

##' Test if \code{x} is a correlation object
##'
##' @param x a correlation object
##' @author David Hajage
##' @keywords internal
is.correlation <- function(x)
  inherits(x, "correlation")
