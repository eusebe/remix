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
    args <- list(...)
    namesargs <- names(args)
    for (i in 1:n) {
      func <- match.fun(fs[[i]])
      forms <- formals(func)
      namesforms <- names(forms)
      if (all(namesforms != "...")) {
        finalargs <- c(list(x = x), args[namesargs %in% namesforms])
      } else {
        finalargs <- c(list(x = x), args)
      }
      tmp <- do.call(func, finalargs)
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
  attr(results, "x") <- x
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
  attr(results, "df") <- df
  results
}

# from http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
vp.layout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
arrange <- function(..., nrow = NULL, ncol = NULL, as.table = FALSE) {
  dots <- list(...)
  n <- length(dots)
  if (is.null(nrow) & is.null(ncol)) {
    nrow <- floor(n/2)
    ncol <- ceiling(n/nrow)
  }
  if (is.null(nrow)) {
    nrow <- ceiling(n/ncol)
  }
  if(is.null(ncol)) {
    ncol <- ceiling(n/nrow)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
  ii.p <- 1
  for(ii.row in seq(1, nrow)) {
    ii.table.row <- ii.row 
    if(as.table) {
      ii.table.row <- nrow - ii.table.row + 1
    }
    for(ii.col in seq(1, ncol)) {
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}

plot.summarize <- function(x, ...) {
  require(ggplot2)
  require(reshape)
  mdf <- melt(attr(x, "df"))
  box <- ggplot(mdf, aes("variable", value)) + geom_boxplot() + facet_grid(. ~ variable) + theme_bw() + opts(axis.text.x = theme_blank(), axis.ticks = theme_blank()) + xlab(NULL)
  dens <- ggplot(mdf, aes(value, ..density..)) + geom_histogram() + geom_density() + facet_grid(. ~ variable) + theme_bw() + opts(axis.ticks = theme_blank()) + xlab(NULL)
  arrange(box, dens, ncol = 1)
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
print.summarize <- function(x, type = "rest", lstyle = "", ...) {
  print(ascii(x, lstyle = lstyle, ...), type = type)
  invisible(x)
}

##' as.data.frame for summarize object.
##'
##' as.data.frame for summarize object (internal).
##'
##' @export
##' @param x a summarize object
##' @param ... not used
##' @author David Hajage
##' @keywords internal
as.data.frame.summarize <- function(x, ...) {
  as.data.frame(unclass(x))
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
##' @param addmargins addmargins
##' @author David Hajage
##' @keywords internal
summarize.by <- function(x, by, funs = c(mean, sd, quantile, n, na), ..., useNA = c("no", "ifany", "always"), addmargins = FALSE) {
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

  if (addmargins) {
    results[[1]] <- c(results[[1]], list(summarize(x, funs = funs, ...)))
    lgroup <- c(lgroup[[1]], "Total")
  }
  
  class(results) <- c("summarize.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  attr(results, "revert") <- FALSE
  
  results
}

##' Compute summary statistics according to a factor (data.frame input)
##'
##' @param df data.frame
##' @param by data.frame
##' @param funs fuctions
##' @param ... passed to funs
##' @param useNA useNA
##' @param addmargins addmargins
##' @param revert whether to regroup factors or numeric variables when crossing factor with numeric variables
##' @author David Hajage
##' @keywords internal
summarize.data.frame.by <- function(df, by, funs = c(mean, sd, quantile, n, na), ..., useNA = c("no", "ifany", "always"), addmargins = FALSE, revert = FALSE) {
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
  dff <- data.frame(df, by, check.names = FALSE)
  names(dff) <- c(names(dff)[1:(length(names(dff))-nby)], namesby)
  results <- NULL
  for (i in 1:nby) {
    dfi <- dff
    if (useNA[1] == "no") {
      dfi <- dfi[!is.na(by[, i]), ]
    }
    results <- c(results, list(dlply(dfi, namesby[i], fun, funs = funs, ...)))
  }
  names(results) <- colnames(by)

  if (addmargins) {
    tot.results <- summarize.data.frame(df, funs = funs, ...)
    results <- lapply(results, function(x) c(x, list(tot.results)))
  }

  r <- lapply(results, function(x) {
    y <- names(x)
    y[is.na(y)] <- "NA"
    if (addmargins)
      y[length(y)] <- "Total"
    y
  })
  
  if (!revert) {
    lgroup <- list(rep(names(df), length(unlist(r))), unlist(r), names(results))
    nr <- nrow(results[[1]][[1]])
    n.lgroup <- list(1, nr, nr*sapply(r, length))
  } else {
    lgroup <- list(unlist(lapply(r, rep, ncol(df))), rep(names(df), ncol(by)), names(results))
    nr <- rep(sapply(results, length), each = length(results))
    nrr <- sapply(results, length)*ncol(by)
    n.lgroup <- list(1, nr, nrr)
  }

  class(results) <- c("summarize.by", "list")
  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  attr(results, "revert") <- revert

  attr(results, "df") <- df
  attr(results, "by") <- by
  
  results
}

plot.summarize.by <- function(x, ...) {
  require(ggplot2)
  require(reshape)
  df <- attr(x, "df")
  by <- attr(x, "by")
  box <- NULL
  dens <- NULL
  for (i in 1:ncol(by)) {
    dfby <- data.frame(df, by = by[, i])
    mdfby <- melt(dfby, id = "by")
    box <- c(box, list(ggplot(mdfby, aes("variable", value)) + geom_boxplot(aes(fill = by)) + facet_grid(~ variable) + theme_bw() + opts(axis.text.x = theme_blank(), axis.ticks = theme_blank()) + scale_fill_discrete(names(by)[i]) + xlab(NULL)))
    dens <- c(dens, list(ggplot(mdfby, aes(value, ..density..)) + geom_histogram(aes(fill = by), position = "dodge") + geom_density(aes(fill = by), position = "dodge") + facet_grid(. ~ variable) + theme_bw() + opts(axis.ticks = theme_blank()) + scale_fill_discrete(names(by)[i]) + xlab(NULL)))
  }
  do.call(arrange, c(box, dens, ncol = ncol(by)))
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
ascii.summarize.by <- function(x, format = "nice", digits = 5, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), ...) {
  if (attr(x, "revert")) {
    xx <- do.call(rbind, lapply(x, function(y) do.call(ascii:::interleave.matrix, y)))
  } else {
    xx <- NULL
    for (i in 1:length(x)) {
      for (j in 1:length(x[[i]])) {
        xx <- rbind(xx, x[[i]][[j]])
      }
    }
  }
  
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
print.summarize.by <- function(x, type = "rest", lstyle = "", ...) {
  print(ascii(x, lstyle = lstyle, ...), type = type)
  invisible(x)
}

##' as.data.frame for summarize.by object.
##'
##' as.data.frame for summarize.by object (internal).
##'
##' @export
##' @param x a summarize.by object
##' @param ... not used
##' @author David Hajage
##' @keywords internal
as.data.frame.summarize.by <- function(x, ...) {
  if (attr(x, "revert")) {
    xx <- do.call(rbind, lapply(x, function(y) do.call(ascii:::interleave.matrix, y)))
    lgroup <- attr(x, "lgroup")
    n.lgroup <- attr(x, "n.lgroup")
    lgroup[[2]] <- unlist(mapply(rep, lgroup[[2]], each = n.lgroup[[2]], SIMPLIFY = FALSE))
    lgroup[[3]] <- unlist(mapply(rep, lgroup[[3]], n.lgroup[[3]], SIMPLIFY = FALSE))
    xx <- data.frame(by = lgroup[[3]], var = lgroup[[2]], levels = lgroup[[1]], xx, row.names = NULL, check.names = FALSE)
  } else {
    xx <- NULL
    for (i in 1:length(x)) {
      for (j in 1:length(x[[i]])) {
        xx <- rbind(xx, x[[i]][[j]])
      }
    }
    lgroup <- attr(x, "lgroup")
    n.lgroup <- attr(x, "n.lgroup")
    lgroup[[2]] <- unlist(mapply(rep, lgroup[[2]], each = n.lgroup[[2]], SIMPLIFY = FALSE))
    lgroup[[3]] <- unlist(mapply(rep, lgroup[[3]], n.lgroup[[3]], SIMPLIFY = FALSE))
    xx <- data.frame(by = lgroup[[3]], levels = lgroup[[2]], var = lgroup[[1]], xx, row.names = NULL, check.names = FALSE)
  }
  xx
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
