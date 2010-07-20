##' ##' Compute a contingency table
##'
##' @param x factor
##' @param y factor
##' @param margin margin
##' @param useNA useNA
##' @param propNA propNA
##' @param addmargins addmargins
##' @author David Hajage
##' @keywords internal
tabular <- function(x, y, margin = 0:2, useNA = c("no", "ifany", "always"), propNA = TRUE, addmargins = FALSE) {
  n <- n.table(x, y, useNA = useNA, margin = margin, addmargins = addmargins)
  p <- p.table(x, y, useNA = useNA, propNA = propNA, margin = margin, addmargins = addmargins)
  rn <- rownames(n)
  rn[is.na(rn)] <- "NA"
  results <- c(n = list(n), p)
  results <- do.call(ascii:::interleave.matrix, results)
  # remove unnecessary rows (all NA)
  results <- results[apply(results, 1, function(x) any(!is.na(x))), ]
  
  attr(results, "lgroup") <- list(gsub("(^n|^cell|^row|^col)(\\.)", "\\1", gsub("(^n\\.|^cell\\.|^row\\.|^col\\.)(.+$)", "\\1", rownames(results))), rownames(n))
  attr(results, "n.lgroup") <- list(1, table(gsub("(^n\\.|^cell\\.|^row\\.|^col\\.)(.+$)", "\\2", rownames(results)))[rn])
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
tabular.data.frame <- function(dfx, dfy, margin = 0:2, useNA = c("no", "ifany", "always"), propNA = TRUE, addmargins = FALSE) {
  results <- lapply(dfy, function(y) lapply(dfx, tabular, y, margin = margin, useNA = useNA, propNA = propNA, addmargins = addmargins))
  
  noms <- names(results[[1]])
  lgroup <- lapply(results[[1]], function(x) attr(x, "lgroup"))

  n.lgroup <- lapply(results[[1]], function(x) attr(x, "n.lgroup"))
  for (i in 1:length(n.lgroup)) {
    n.lgroup[[i]] <- c(n.lgroup[[i]], sum(n.lgroup[[i]][[2]]))
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

  attr(results, "dfx") <- dfx
  attr(results, "dfy") <- dfy
  
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
