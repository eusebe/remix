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

n <- function(x, na.rm = FALSE) {
  sum(!is.na(x))
}

na <- function(x, na.rm = FALSE) {
  sum(is.na(x))
}

resume <- function(x, funs = c(mean, sd, quantile, n, na), ...) {
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
  class(results) <- c("resume", "matrix")
  results
}

resume.data.frame <- function(df, funs = c(mean, sd, quantile, n, na), ...) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  dfl <- as.list(df)
  results <- t(sapply(dfl, resume, funs = funs, ...))
  class(results) <- c("resume", "matrix")
  results
}

ascii.resume <- function(x, format = "fg", digits = 5, ...) {
  if (is.null(nrow(x))) {
    x <- t(x)
  }
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, format = format, digits = digits, ...)
}

print.resume <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

is.resume <- function(x)
  inherits(x, "resume")

resume.by <- function(x, by, funs = c(mean, sd, quantile, n, na), ...) {
  if (!is.numeric(x))
    stop("x doit etre numerique")  

  by <- as.character(by)
  by[is.na(by)] <- "NA"
  
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  fun <- function(df, funs = funs, ...) resume(df[, 1], funs = funs, ...)
  df <- data.frame(x, by, check.names = FALSE)
  names(df) <- c(names(df)[1], "by")
  results <- list(dlply(df, .(by), fun, funs = funs, ...))

  lgroup <- lapply(results, function(x) names(x))
  n.lgroup <- NULL

  class(results) <- c("resume.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  
  results
}

resume.data.frame.by <- function(df, by, funs = c(mean, sd, quantile), ...) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  by <- sapply(by, as.character)
  by[is.na(by)] <- "NA"

  nby <- ncol(by)
  namesby <- paste("by", 1:nby, sep = ".")
  fun <- function(df, funs = funs, ...) resume.data.frame(df[, 1:(ncol(df)-nby), drop = FALSE], funs = funs, ...)
  df <- data.frame(df, by, check.names = FALSE)
  names(df) <- c(names(df)[1:(length(names(df))-nby)], namesby)
  results <- NULL
  for (i in 1:nby) {
    results <- c(results, list(dlply(df, namesby[i], fun, funs = funs, ...)))
  }
  names(results) <- colnames(by)

  r <- lapply(results, function(x) names(x))
  lgroup <- list(unlist(r), names(results))
  nr <- nrow(results[[1]][[1]])
  n.lgroup <- list(nr, nr*sapply(r, length))

  class(results) <- c("resume.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  
  results
}

ascii.resume.by <- function(x, format = "fg", digits = 5, ...) {
  xx <- NULL
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])) {
      xx <- rbind(xx, x[[i]][[j]])
    }
  }

  include.rownames = TRUE
  if (is.null(attr(x, "n.lgroup")))
    include.rownames = FALSE

  class(x) <- class(x)[-1]
  ascii:::ascii(xx, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), include.rownames = include.rownames, include.colnames = TRUE, header = TRUE, format = format, digits = digits, ...)
}

print.resume.by <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

is.resume.by <- function(x)
  inherits(x, "resume.by")

correlation <- function(x, y, method = c("pearson", "kendall", "spearman")) {
  results <- cor.test(x, y, method = method)$estimate
  class(results) <- c("correlation", "matrix")
  results
}

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

ascii.correlation <- function(x, format = "fg", digits = 5, ...) {
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.rownames = TRUE, include.colnames = TRUE, header = TRUE, format = format, digits = digits, ...)
}

print.correlation <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

is.correlation <- function(x)
  inherits(x, "correlation")
