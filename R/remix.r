cross <- function(x, y = NULL, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, useNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman")) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  results <- "What?"
  
  if (!is.null(x) & !is.null(y)) {
    if (all(sapply(x, is.numeric)) & all(!sapply(y, is.numeric))) {
      results <- resume.data.frame.by(x, y, funs = funs, ..., useNA = useNA)
    }
    if (all(sapply(y, is.numeric)) & all(!sapply(x, is.numeric))) {
      results <- resume.data.frame.by(y, x, funs = funs, ..., useNA = useNA)
    }
    if (all(!sapply(x, is.numeric)) & all(!sapply(y, is.numeric))) {
      results <- tabular.data.frame(x, y, margin = margin, useNA = useNA)
    }
    if (all(sapply(x, is.numeric)) & all(sapply(y, is.numeric))) {
      results <- correlation.data.frame(x, y, method = method)
    }
  } else if (is.null(y)) {
    if (all(!sapply(x, is.numeric))) {
      results <- freq.data.frame(x, useNA = useNA, cum = cum)
    }
    if (all(sapply(x, is.numeric))) {
      results <- resume.data.frame(x, funs = funs, ...)
    }
  } else if (is.null(x)) {
    if (all(!sapply(y, is.numeric))) {
      results <- freq.data.frame(y, useNA = useNA, cum = cum)
    } 
    if (all(sapply(y, is.numeric))) {
      results <- resume.data.frame(y, funs = funs, ...)
    }
  }

  attr(results, "split_type") <- NULL
  attr(results, "split_labels") <- NULL
  
  results
}

cross_list <- function(l, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, useNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman")) {

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

  cross(x = x, y = y, funs = funs, ..., cum = cum, margin = margin, useNA = useNA, method = method)
}

remix <- function(formula, data, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, useNA = "no", method = c("pearson", "kendall", "spearman")) {
  
  if (is.formula(formula))
    formula <- deparse(formula, 500)
  
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }
  
  varnames <- names(data)
  parsed <- parse_formula(formula, varnames)
  
  data <-   parse_data(expand_formula(formula, varnames), data)
  varform <- names(data)
  
  eg <- expand.grid(parsed$left, parsed$right)
  
  if (all(parsed$by == ".")) {
    comb <- lapply(apply(eg, 1, list), function(x) {
      y <- unlist(x)
      y <- y[y != "."]
      y <- sub("(c\\()(.*)(\\))", "\\2", y)
      lapply(y, function(z) data[, strsplit(z, ",")[[1]], drop = FALSE])})
    
    results <- llply(comb, cross_list, funs = funs, ..., cum = cum, margin = margin, useNA = useNA, method = method)
    names(results) <- apply(eg, 1, paste, collapse = " ~ ")
  }
  
  if (any(parsed$by != ".")) {
    databy <- list()
    for (i in 1:length(parsed$by)) {
      ldata <- dlply(data, parsed$by[i], function(x){x})
      attr(ldata, "split_type") <- NULL
      attr(ldata, "split_labels") <- NULL
      
      databy <- c(databy, list(ldata))
    }
    names(databy) <- parsed$by

    combby <- lapply(databy, function(o) lapply(o, function(p) lapply(apply(eg, 1, list), function(x) {
      y <- unlist(x)
      y <- y[y != "."]
      y <- sub("(c\\()([^\\)]*)(\\))", "\\2", y)
      lapply(y, function(z) p[, strsplit(z, ",")[[1]], drop = FALSE])})))

    results <- lapply(combby, function(x) lapply(x, function(comb) {
      res <- llply(comb, cross_list, funs = funs, cum = cum, margin = margin, useNA = useNA, method = method)
      names(res) <- apply(eg, 1, paste, collapse = " ~ ")
      res
    }))
    if (length(results) == 1)
      results <- results[[1]]
  }
  
  class(results) <- c("remix")
  attr(results, "formula") <- formula
  attr(results, "data") <- data
  results
}

ascii.remix <- function(x, caption.level = "m", format = "nice", digits = 5, ...) {
  captions <- names(x)
  xx <- NULL
  for (i in 1:length(x)) {
    xx <- ascii:::asciiMixed$new(xx, ascii(x[[i]], caption = captions[i], caption.level = caption.level, format = format, digits = digits, ...))
    class(xx) <- c("ascii", "proto", "environment")
  }
  xx
}

print.remix <- function(x, type = "rest", caption.level = NULL, ...) {
  print(ascii(x, caption.level = caption.level, ...), type = "rest")
  invisible(x)
}

is.remix <- function(x)
    inherits(x, "remix")
