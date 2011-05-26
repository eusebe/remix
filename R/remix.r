##' Cross x and y
##'
##' @param x x
##' @param y y
##' @param funs funs
##' @param ... \dots
##' @param cum cum
##' @param margin margin
##' @param addmargins addmargins
##' @param useNA useNA
##' @param propNA propNA
##' @param revert whether to regroup factors or numeric variables when crossing factor with numeric variables
##' @param method method
##' @param test test
##' @param test.tabular test.tabular
##' @param test.summarize test.summarize
##' @param show.test show.test
##' @author David Hajage
##' @keywords internal
cross <- function(x, y = NULL, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, addmargins = FALSE, useNA = c("no", "ifany", "always"), propNA = TRUE, revert = FALSE, method = c("pearson", "kendall", "spearman"), test = FALSE, test.tabular = test.tabular.auto, test.summarize = test.summarize.auto, show.test = display.test, plim = 4, show.method = TRUE) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  results <- "What?"
  
  if (!is.null(x) & !is.null(y)) {
    if (all(sapply(x, is.numeric)) & all(!sapply(y, is.numeric))) {
      results <- summarize.data.frame.by(x, y, funs = funs, ..., addmargins = addmargins, useNA = useNA, revert = revert, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method)
    }
    if (all(sapply(y, is.numeric)) & all(!sapply(x, is.numeric))) {
      results <- summarize.data.frame.by(y, x, funs = funs, ..., addmargins = addmargins, useNA = useNA, revert = revert, test = test, test.summarize = test.summarize, show.test = show.test, plim = plim, show.method = show.method)
    }
    if (all(!sapply(x, is.numeric)) & all(!sapply(y, is.numeric))) {
      results <- tabular.data.frame(x, y, margin = margin, addmargins = addmargins, useNA = useNA, propNA = propNA, test = test, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method)
    }
    if (all(sapply(x, is.numeric)) & all(sapply(y, is.numeric))) {
      results <- correlation.data.frame(x, y, method = method)
    }
  } else if (is.null(y)) {
    if (all(!sapply(x, is.numeric))) {
      results <- freq.data.frame(x, addmargins = addmargins, useNA = useNA, propNA = propNA, cum = cum)
    }
    if (all(sapply(x, is.numeric))) {
      results <- summarize.data.frame(x, funs = funs, ...)
    }
  } else if (is.null(x)) {
    if (all(!sapply(y, is.numeric))) {
      results <- freq.data.frame(y, addmargins = addmargins, useNA = useNA, propNA = propNA, cum = cum)
    } 
    if (all(sapply(y, is.numeric))) {
      results <- summarize.data.frame(y, funs = funs, ...)
    }
  }

  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  
  results
}

##' Cross variables in a list
##'
##' @param l 
##' @param funs funs
##' @param ... \dots
##' @param cum cum
##' @param margin margin
##' @param addmargins addmargins
##' @param useNA useNA
##' @param propNA 
##' @param revert whether to regroup factors or numeric variables when crossing factor with numeric variables
##' @param method method
##' @param test test
##' @param test.summarize test.summarize
##' @param test.tabular test.tabular
##' @param show.test show.test
##' @param x x
##' @param y y
##' @author David Hajage
##' @keywords internal
cross_list <- function(l, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, addmargins = FALSE, useNA = c("no", "ifany", "always"), propNA = TRUE, revert = FALSE, method = c("pearson", "kendall", "spearman"), test = FALSE, test.summarize = test.summarize.auto, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE) {

  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }
  
  x <- l[[1]]
  if (length(l) == 2) {
    y <- l[[2]]
  } else {
    y <- NULL
  }

  cross(x = x, y = y, funs = funs, ..., cum = cum, margin = margin, addmargins = addmargins, useNA = useNA, propNA = propNA, revert = revert, method = method, test = test, test.summarize = test.summarize, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method)
}

##' Regroup factors with factors, and numerical variables with numerical variables
##'
##' @param vars vars
##' @param numdata numdata
##' @author David Hajage
##' @keywords internal
regroup <- function(vars, numdata) {
  vars <- lapply(strsplit(sub("(cbind\\()(.*)(\\))", "\\2", vars), ","), remove_blank)
  results <- unlist(lapply(vars, function(x) {
    if (!all(x %in% numdata) | !all(!(x %in% numdata))) {
      if (length(x[x %in% numdata]) == 1)
        xx <- x[x %in% numdata]
      else 
        xx <- paste("cbind(", paste(x[x %in% numdata], collapse = ","), ")", sep = "")
      if (length(x[!(x %in% numdata)]) == 1)
        xx <- c(xx, x[!(x %in% numdata)])
      else 
        xx <- c(xx, paste("cbind(", paste(x[!(x %in% numdata)], collapse = ","), ")", sep = ""))
    } else {
      if (length(x) == 1)
        xx <- x
      else
        xx <- paste("cbind(", paste(x, collapse = ","), ")", sep = "")
    }
    xx[xx != "cbind()"]
  }))
  results
}

##' Remix and describe.
##'
##' A quick and easy function for describing datasets.
##'
##' @param formula a formula (see Details).
##' @param data a data.frame.
##' @param funs functions for describing numeric variable.   Can be
##' \code{c(fun1, fun2, fun3)} or   \code{c("fun1", "fun2", "fun3")}
##' or a list.
##' @param ... further arguments (all passed to funs), for example
##' \code{na.rm = TRUE}\dots.
##' @param cum should cumulated frequencies be reported?
##' @param margin index, or vector of indices to generate proportion
##' in frequency tables (0: cell, 1: row, 2: col).
##' @param addmargins whether to add margins
##' @param useNA whether to include NA as a level (factor)
##' @param propNA whether to include NA in proportion calculation
##' @param revert whether to regroup factors or numeric variables when crossing factor with numeric variables
##' @param method a character string indicating which correlation
##' coefficient is to be   used. One of \code{"pearson"},
##' \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
##' @param test should test?
##' @param test.summarize function used to compare means
##' @param test.tabular function used to test association betwean two factors
##' @param show.test function used to display the test
##' @param plim number of digits of the p value
##' @param show.method should show the test method?
##' @note
##'   The formula has the following format: \code{x_1 + x_2 + ... ~ y_1 + y_2 + ...}
##'
##'   There are a couple of special variables: \code{...} represents all
##'   other variables not used in the formula and \code{.} represents no
##'   variable, so you can do \code{formula = var1 ~ .}.
##'
##'   If \code{var1} is numeric, \code{var1 ~ .} produce a summary table
##'   using \code{funs}. If \code{var1} is a factor, \code{var1 ~ .} produce
##'   a frequency table. If \code{var1} is numeric and \code{var2} is
##'   numeric, \code{var1 ~ var2} gives correlation. if \code{var1} is
##'   numeric and \code{var2} is a factor, \code{var1 ~ var2} produce a
##'   summary table using \code{funs} according to the levels of
##'   \code{var2}. If \code{var1} is a factor and \code{var2} is a factor,
##'   \code{var1 ~ var2} produce a contingency table.
##'
##'   You can group several variables of the same type (numeric or factor)
##'   together with \code{cbind(var1, var2, var3)}, they will be grouped in the
##'   same table. \code{cbind(...)} works (ie regroups all variables of the same
##'   type).
##' @return
##'   A remix object, basically a list with descriptive tables. It uses
##'   \code{ascii} package for printing output, and can be use with
##'   \code{ascii} function.
##' @author David Hajage, inspired by the design and the code of
##'   \code{summary.formula} (\code{Hmisc} package, FE Harrell) and
##'   \code{cast} (\code{reshape} package, H Wickham).
##' @seealso \code{cast} (reshape) and \code{summary.formula} (Hmisc).
##' @examples
##' parwidth <- getOption("width")
##' options(width = 100)
##'
##' library(remix)
##' remix(data = iris)
##' remix(cbind(...) ~ ., iris[, sapply(iris, is.numeric)], funs = c(median, mad, min, max))
##' remix(cbind(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs = quantile, probs = c(1/3, 2/3))
##' remix(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris)
##' remix(cbind(Sepal.Length, Sepal.Width) ~ cbind(Petal.Length, Petal.Width), iris)
##' remix(... ~ ., esoph, cum = TRUE)
##' remix(alcgp ~ tobgp, esoph, cum = TRUE)
##' 
##' options(width = parwidth)
##' @keywords univar
##' @export
remix <- function(formula = cbind(...) ~ ., data = NULL, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, addmargins = FALSE, useNA = c("no", "ifany", "always"), propNA = TRUE, revert = FALSE, method = c("pearson", "kendall", "spearman"), test = FALSE, test.summarize = test.summarize.auto, test.tabular = test.tabular.auto, show.test = display.test, plim = 4, show.method = TRUE) {
  
  if (is.formula(formula))
    formula <- deparse(formula, 500)
  
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }
  
  varnames <- names(data)
  parsed <- parse_formula(formula, varnames)
  
  data <-   parse_data(expand_formula(formula, varnames), data)
  names(data) <- remove_blank(names(data))
  varform <- names(data)

  numdata <- varform[sapply(data, is.numeric)]
  parsed$left <- regroup(parsed$left, numdata)
  parsed$right <- regroup(parsed$right, numdata)
  
  eg <- expand.grid(parsed$left, parsed$right)
  
  ## if (all(parsed$by == ".")) {
    comb <- lapply(apply(eg, 1, list), function(x) {
      y <- unlist(x)
      y <- y[y != "."]
      y <- sub("(cbind\\()(.*)(\\))", "\\2", y)
      lapply(y, function(z) data[, strsplit(z, ",")[[1]], drop = FALSE])})
    
    results <- llply(comb, cross_list, funs = funs, ..., cum = cum, margin = margin, addmargins = addmargins, useNA = useNA, propNA = propNA, revert = revert, method = method, test = test, test.summarize = test.summarize, test.tabular = test.tabular, show.test = show.test, plim = plim, show.method = show.method)
    names(results) <- apply(eg, 1, paste, collapse = " ~ ")
  ## }
  
  ## if (any(parsed$by != ".")) {
  ##   databy <- list()
  ##   for (i in 1:length(parsed$by)) {
  ##     ldata <- dlply(data, parsed$by[i], function(x){x})
  ##     attr(ldata, "split_type") <- NULL
  ##     attr(ldata, "split_labels") <- NULL
      
  ##     databy <- c(databy, list(ldata))
  ##   }
  ##   names(databy) <- parsed$by

  ##   combby <- lapply(databy, function(o) lapply(o, function(p) lapply(apply(eg, 1, list), function(x) {
  ##     y <- unlist(x)
  ##     y <- y[y != "."]
  ##     y <- sub("(cbind *\\()(.*)(\\))", "\\2", y)
  ##     lapply(y, function(z) p[, strsplit(z, ",")[[1]], drop = FALSE])})))

  ##   results <- lapply(combby, function(x) lapply(x, function(comb) {
  ##     res <- llply(comb, cross_list, funs = funs, cum = cum, margin = margin, addmargins = addmargins, useNA = useNA, propNA = propNA, revert = revert, method = method)
  ##     names(res) <- apply(eg, 1, paste, collapse = " ~ ")
  ##     res
  ##   }))
  ##   if (length(results) == 1)
  ##     results <- results[[1]]
  ## }
  
  class(results) <- c("remix")
  attr(results, "formula") <- formula
  attr(results, "left") <- parsed$left
  attr(results, "right") <- parsed$right
  ## attr(results, "by") <- parsed$by
  
  attr(results, "data") <- data
  results
}

##' Ascii for remix object.
##'
##' Ascii method for remix object.
##'
##' @export
##' @param x a remix object
##' @param caption.level see \code{?ascii} in \code{ascii} package
##' @param format see \code{?ascii} in \code{ascii} package
##' @param digits see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii} (all except \code{caption}
##'    which has no effect)
##' @author David Hajage
##' @keywords univar
ascii.remix <- function(x, caption.level = c("s", "e", "m"), format = "nice", digits = 2, ...) {
  caption.level <- rep(caption.level, length = 3)
  caption.level1 <- caption.level[1]
  caption.level2 <- caption.level[2]
  caption.level3 <- caption.level[3]
  
  xx <- ascii:::asciiMixed$new(NULL)
  class(xx) <- c("ascii", "proto", "environment")
  ## if (all(attr(x, "by") == ".")) {
    captions <- names(x)
    for (i in 1:length(x)) {
      xx[[paste("obj", i, sep = "")]] <- ascii(x[[i]], caption = captions[i], caption.level = caption.level1, format = format, digits = digits, ...)
    }
  ## } else if (length(attr(x, "by")) == 1) {
  ##   captions1 <- names(x)
  ##   captions2 <- names(x[[1]])
  ##   for (i in 1:length(x)) {
  ##     asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
  ##     xx[[paste("obj", i, sep = "")]] <- asc.cap1
  ##     for (j in 1:length(x[[i]])) {
  ##       xx[[paste("obj", i, j, sep = "")]] <- ascii(x[[i]][[j]], caption = captions2[j], caption.level = caption.level2, format = format, digits = digits, ...)
  ##     }
  ##   }
  ## } else if (length(attr(x, "by")) > 1) {
  ##   captions1 <- names(x)
  ##   captions3 <- names(x[[1]][[1]])
  ##   for (i in 1:length(x)) {
  ##     asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
  ##     xx[[paste("obj", i, sep = "")]] <- asc.cap1
  ##     for (j in 1:length(x[[i]])) {
  ##       captions2 <- names(x[[i]])
  ##       asc.cap2 <- ascii(list(NULL), caption = captions2[j], caption.level = caption.level2)
  ##       xx[[paste("obj", i, j, sep = "")]] <- asc.cap2
  ##       for (k in 1:length(x[[i]][[j]])) {
  ##         xx[[paste("obj", i, j, k, sep = "")]] <- ascii(x[[i]][[j]][[k]], caption = captions3[k], caption.level = caption.level3, format = format, digits = digits)
  ##       }
  ##     }
  ##   }
  ## }
  xx
}

##' Print a remix object
##'
##' Print remix object using ascii package
##'
##' @export
##' @param x a remix object
##' @param type type of output. See \code{?ascii} in \code{ascii} package
##' @param caption.level see \code{?ascii} in \code{ascii} package
##' @param lstyle see \code{?ascii} in \code{ascii} package
##' @param tstyle see \code{?ascii} in \code{ascii} package
##' @param ... other arguments passed to \code{ascii} (all except \code{caption}
##'    which has no effect)
##' @author David Hajage
##' @keywords univar
print.remix <- function(x, type = "rest", caption.level = 1:3, lstyle = "", tstyle = "", ...) {
  print(ascii(x, caption.level = caption.level, lstyle = lstyle, tstyle = tstyle, ...), type = type)
  invisible(x)
}

##' Demix
##'
##' Transfrom a remix object into a (list of) data.frame(s).
##'
##' @export
##' @param x a remix object
##'
##' @return
##'   A list of data.frame.
##' @author David Hajage
##' @seealso \code{remix}
##' @examples
##'   x <- remix(... ~ ., esoph, cum = TRUE)
##'   demix(x)
demix <- function(x) {
  ## if (all(attr(x, "by") == ".")) {
    result <- lapply(x, as.data.frame)
  ## } else if (length(attr(x, "by")) == 1) {
  ##   result <- lapply(x, function(x) lapply(x, as.data.frame))
  ## } else if (length(attr(x, "by")) > 1) {
  ##   result <- lapply(x, function(x) lapply(x, function(x) lapply(x, as.data.frame)))
  ## }
  return(result)
}

##' Test if \code{x} is an remix object
##'
##' Test if \code{x} is an remix object
##'
##' @param x a remix object
##' @author David Hajage
##' @keywords internal
is.remix <- function(x)
    inherits(x, "remix")
