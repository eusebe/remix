##' \code{prop.table} with cell and propNA
##'
##' @param ... ...
##' @param useNA useNA
##' @param margin margin
##' @param propNA propNA
##' @author David Hajage
##' @keywords internal
prop.table2 <- function (..., useNA = c("no", "ifany", "always"), margin = 0, propNA = TRUE) {
  n <- table(..., useNA = useNA)
  nn <- n
  if (!propNA) {
    if (length(dim(nn)) == 2) {
      nn[is.na(rownames(nn)), ] <- 0
      nn[, is.na(colnames(nn))] <- 0
    } else {
      nn[is.na(names(nn))] <- 0
    }
  }
  if (margin != 0) 
      prop <- sweep(nn, margin, margin.table(nn, margin), "/", check.margin = FALSE)
    else
      prop <- nn/sum(nn)
  if (!propNA) {
    if (length(dim(prop)) == 2) {
      prop[is.na(rownames(prop)), ] <- NA
      prop[, is.na(colnames(prop))] <- NA
    } else {
      prop[is.na(names(prop))] <- NA
    }
  }
  list(n = n, p = prop)
}

##' Compute frequencies
##'
##' @param x factor
##' @param useNA useNA
##' @param propNA propNA
##' @param cum logical
##' @author David Hajage
##' @keywords internal
freq <- function(x, useNA = c("no", "ifany", "always"), propNA = TRUE, cum = FALSE) {
  rnames <- as.character(as.list(substitute(list(x)))[-1])
  np <- prop.table2(x, useNA = useNA, propNA = propNA)
  n <- np$n
  p <- np$p
  if (cum) {
    n.cum <- cumsum(n)
    p.cum <- cumsum(p)
  } else {
    n.cum <- NULL
    p.cum <- NULL
  }
  results <- cbind(n, p, n.cum, p.cum)
  attr(results, "lgroup") <- list(names(n), rnames)
  attr(results, "n.lgroup") <- list(1, nrow(results))
  class(results) <- c("freq", "matrix")
  results
}

##' Compute frequencies (data.frame input)
##'
##' @param df data.frame
##' @param useNA useNA
##' @param propNA propNA
##' @param cum logical
##' @author David Hajage
##' @keywords internal
freq.data.frame <- function(df, useNA = c("no", "ifany", "always"), propNA = TRUE, cum = FALSE) {
  dfl <- as.list(df)
  rnames <- names(dfl)
  results <- lapply(dfl, freq, useNA = useNA, propNA = propNA, cum = cum)
  nrows <- sapply(results, nrow)
  results <- rbind.list(results)

  attr(results, "n.lgroup") <- list(1, nrows)
  attr(results, "lgroup") <- list(unlist(lapply(df, function(x) {
    if (useNA[1] == "always" | ((useNA[1] == "ifany") & any(is.na(x))))
      levels(addNA(x))
    else 
      levels(x)})), rnames)
  class(results) <- c("freq", "matrix")
  results
}

##' Ascii for freq object.
##'
##' Ascii method for freq object (internal).
##'
##' @export
##' @param x a freq object
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param include.rownames see \code{?ascii} in \code{ascii} package
##' @param rownames see \code{?ascii} in \code{ascii} package
##' @param include.colnames see \code{?ascii} in \code{ascii} package
##' @param header see \code{?ascii} in \code{ascii} package
##' @param lgroup see \code{?ascii} in \code{ascii} package
##' @param n.lgroup see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
ascii.freq <- function(x, format = "nice", digits = 3, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), ...) {
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.colnames = include.colnames, include.rownames = include.rownames, header = header, lgroup = lgroup, n.lgroup = n.lgroup, format = format, digits = digits, ...)
}

##' Print freq object.
##'
##' Print freq object (internal).
##'
##' @export
##' @param x a freq object
##' @param type type of output (see \code{?ascii} in \code{ascii}
##' package)
##' @param lstyle see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
print.freq <- function(x, type = "rest", lstyle = "", ...) {
  print(ascii(x, lstyle = lstyle, ...), type = type)
  invisible(x)
}

##' as.data.frame for freq object.
##'
##' as.data.frame for freq object (internal).
##'
##' @export
##' @param x a freq object
##' @param ... not used
##' @author David Hajage
##' @keywords internal
as.data.frame.freq <- function(x, ...) {
  xx <- unclass(x)
  var <- unlist(mapply(rep, attr(x, "lgroup")[[2]], attr(x, "n.lgroup")[[2]], SIMPLIFY = FALSE))
  levels <- attr(x, "lgroup")[[1]]
  
  data.frame(var = var, levels = levels, xx, row.names = NULL, check.names = FALSE)
}

##' Test if \code{x} is an freq object
##'
##' @export
##' @param x a freq object
is.freq <- function(x)
  inherits(x, "freq")

##' Compute a contingency table
##'
##' @param x factor
##' @param y factor
##' @param margin margin
##' @param useNA useNA
##' @param propNA propNA
##' @author David Hajage
##' @keywords internal
tabular <- function(x, y, margin = 0:2, useNA = c("no", "ifany", "always"), propNA = TRUE) {
  n <- table(x, y, useNA = useNA)
  ptable <- function(x, y, useNA, margin, propNA) {
    prop.table2(x, y, useNA = useNA, margin = margin, propNA = propNA)$p
  }
  p <- mapply(ptable, x = list(x), y = list(y), margin = margin, useNA = useNA[1], propNA = propNA, SIMPLIFY = FALSE)
  names(p) <- sapply(as.character(margin), function(x) switch(x, "0" = "cell", "1" = "row", "2" = "col"))
  results <- c(n = list(n), p)
  results <- do.call(ascii:::interleave.matrix, results)
  attr(results, "lgroup") <- list(rep(c("n", names(p)), nrow(n)), rownames(n))
  attr(results, "n.lgroup") <- list(1, 1+length(p))
  attr(results, "tgroup") <- NULL
  attr(results, "n.tgroup") <- NULL
  class(results) <- c("tabular", "matrix")
  results
}

##' Compute a contingency table (data.frame input)
##'
##' @param dfx data.frame
##' @param dfy data.frame
##' @param margin margin
##' @param useNA useNA
##' @param propNA propNA
##' @author David Hajage
##' @keywords internal
tabular.data.frame <- function(dfx, dfy, margin = 0:2, useNA = c("no", "ifany", "always"), propNA = TRUE) {
  results <- lapply(dfy, function(y) lapply(dfx, tabular, y, margin = margin, useNA = useNA, propNA = propNA))
  
  noms <- names(results[[1]])
  lgroup <- lapply(results[[1]], function(x) attr(x, "lgroup"))

  n.lgroup <- lapply(results[[1]], function(x) attr(x, "n.lgroup"))
  for (i in 1:length(n.lgroup)) {
    n.lgroup[[i]] <- c(n.lgroup[[i]], length(lgroup[[i]][[2]])*length(unique(lgroup[[i]][[1]])))
  }
  n.lgroup1 <- unlist(lapply(n.lgroup, function(x) x[[1]]))
  n.lgroup2 <- unlist(lapply(n.lgroup, function(x) x[[2]]))
  n.lgroup3 <- unlist(lapply(n.lgroup, function(x) x[[3]]))
  n.lgroup <- list(n.lgroup1, n.lgroup2, n.lgroup3)

  for (i in 1:length(lgroup)) {
    lgroup[[i]] <- c(lgroup[[i]], noms[i])
  }
  lgroup1 <- unlist(lapply(lgroup, function(x) x[[1]]))
  lgroup2 <- unlist(lapply(lgroup, function(x) x[[2]]))
  lgroup3 <- unlist(lapply(lgroup, function(x) x[[3]]))
  lgroup <- list(lgroup1, lgroup2, lgroup3)

  tgroup <- names(results)
  n.tgroup <- unlist(lapply(results, function(x) ncol(x[[1]])))

  results <- lapply(results, rbind.list)
  results <- cbind.list(results)

  attr(results, "lgroup") <- lgroup
  attr(results, "n.lgroup") <- n.lgroup
  attr(results, "tgroup") <- tgroup
  attr(results, "n.tgroup") <- n.tgroup
  
  class(results) <- c("tabular", "matrix")
  results
}

##' Ascii for tabular object.
##'
##' Ascii method for tabular object (internal).
##'
##' @export
##' @param x a tabular object
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param include.rownames see \code{?ascii} in \code{ascii} package
##' @param include.colnames see \code{?ascii} in \code{ascii} package
##' @param header see \code{?ascii} in \code{ascii} package
##' @param lgroup see \code{?ascii} in \code{ascii} package
##' @param n.lgroup see \code{?ascii} in \code{ascii} package
##' @param tgroup see \code{?ascii} in \code{ascii} package
##' @param n.tgroup see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
ascii.tabular <- function(x, format = "nice", digits = 5, include.rownames = FALSE, include.colnames = TRUE, header = TRUE, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), tgroup = attr(x, "tgroup"), n.tgroup = attr(x, "n.tgroup"), ...) {
  class(x) <- class(x)[-1]

  ascii:::ascii(x, lgroup = lgroup, n.lgroup = n.lgroup, tgroup = tgroup, n.tgroup = n.tgroup, include.rownames = include.rownames, include.colnames = include.colnames, header = header, format = format, digits = digits, ...)
}

##' Print tabular object.
##'
##' Print tabular object (internal).
##'
##' @export
##' @param x a tabular object
##' @param type type of output (see \code{?ascii} in \code{ascii}
##' package)
##' @param lstyle see \code{?ascii} in \code{ascii} package
##' @param tstyle see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii}
##' @author David Hajage
##' @keywords univar
print.tabular <- function(x, type = "rest", lstyle = "", tstyle = "", ...) {
  print(ascii:::ascii(x, lstyle = lstyle, tstyle = tstyle, ...), type = type)
  invisible(x)
}

##' as.data.frame for tabular object.
##'
##' as.data.frame for tabular object (internal).
##'
##' @export
##' @param x a tabular object
##' @param ... not used
##' @author David Hajage
##' @keywords internal
as.data.frame.tabular <- function(x, ...) {
  xx <- unclass(x)
  var <- unlist(mapply(rep, attr(x, "lgroup")[[3]], attr(x, "n.lgroup")[[3]], SIMPLIFY = FALSE))
  levels <- unlist(mapply(rep, attr(x, "lgroup")[[2]], attr(x, "n.lgroup")[[2]], SIMPLIFY = FALSE))
  margin <- attr(x, "lgroup")[[1]]
  
  data.frame(var = var, levels = levels, margin = margin, xx, row.names = NULL, check.names = FALSE)
}

##' Test if \code{x} is an tabular object
##'
##' @param x a tabular object
##' @author David Hajage
##' @keywords internal
is.tabular <- function(x)
  inherits(x, "tabular")
