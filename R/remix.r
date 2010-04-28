cross <- function(x, y = NULL, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, useNA = c("no", "ifany", "always"), method = c("pearson", "kendall", "spearman")) {
  if (!is.character(funs)) {
    funs <- as.character(as.list(substitute(funs)))
    funs <- funs[funs != "c" & funs != "list"]
  }

  results <- "What?"
  
  if (!is.null(x) & !is.null(y)) {
    if (all(sapply(x, is.numeric)) & all(!sapply(y, is.numeric))) {
      results <- summarize.data.frame.by(x, y, funs = funs, ..., useNA = useNA)
    }
    if (all(sapply(y, is.numeric)) & all(!sapply(x, is.numeric))) {
      results <- summarize.data.frame.by(y, x, funs = funs, ..., useNA = useNA)
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
      results <- summarize.data.frame(x, funs = funs, ...)
    }
  } else if (is.null(x)) {
    if (all(!sapply(y, is.numeric))) {
      results <- freq.data.frame(y, useNA = useNA, cum = cum)
    } 
    if (all(sapply(y, is.numeric))) {
      results <- summarize.data.frame(y, funs = funs, ...)
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

regroup <- function(vars, numdata) {
  vars <- strsplit(sub("(cbind\\()(.*)(\\))", "\\2", vars), ",")
  unlist(lapply(vars, function(x) {
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
}

remix <- function(formula = cbind(...) ~ ., data, funs = c(mean, sd, quantile, n, na), ..., cum = FALSE, margin = 0:2, useNA = "no", method = c("pearson", "kendall", "spearman")) {
  
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

  numdata <- varform[sapply(data, is.numeric)]
  parsed$left <- regroup(parsed$left, numdata)
  parsed$right <- regroup(parsed$right, numdata)
  
  eg <- expand.grid(parsed$left, parsed$right)
  
  if (all(parsed$by == ".")) {
    comb <- lapply(apply(eg, 1, list), function(x) {
      y <- unlist(x)
      y <- y[y != "."]
      y <- sub("(cbind\\()(.*)(\\))", "\\2", y)
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
      y <- sub("(cbind *\\()(.*)(\\))", "\\2", y)
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
  attr(results, "left") <- parsed$left
  attr(results, "right") <- parsed$right
  attr(results, "by") <- parsed$by
  
  attr(results, "data") <- data
  results
}

ascii.remix <- function(x, caption.level = c("s", "e", "m"), format = "nice", digits = 5, ...) {
  caption.level <- rep(caption.level, length = 3)
  caption.level1 <- caption.level[1]
  caption.level2 <- caption.level[2]
  caption.level3 <- caption.level[3]
  
  xx <- ascii:::asciiMixed$new(NULL)
  class(xx) <- c("ascii", "proto", "environment")
  if (all(attr(x, "by") == ".")) {
    captions <- names(x)
    for (i in 1:length(x)) {
      xx[[paste("obj", i, sep = "")]] <- ascii(x[[i]], caption = captions[i], caption.level = caption.level1, format = format, digits = digits, ...)
    }
  } else if (length(attr(x, "by")) == 1) {
    captions1 <- names(x)
    captions2 <- names(x[[1]])
    for (i in 1:length(x)) {
      asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
      xx[[paste("obj", i, sep = "")]] <- asc.cap1
      for (j in 1:length(x[[i]])) {
        xx[[paste("obj", i, j, sep = "")]] <- ascii(x[[i]][[j]], caption = captions2[j], caption.level = caption.level2, format = format, digits = digits, ...)
      }
    }
  } else if (length(attr(x, "by")) > 1) {
    captions1 <- names(x)
    captions3 <- names(x[[1]][[1]])
    for (i in 1:length(x)) {
      asc.cap1 <- ascii(list(NULL), caption = captions1[i], caption.level = caption.level1)
      xx[[paste("obj", i, sep = "")]] <- asc.cap1
      for (j in 1:length(x[[i]])) {
        captions2 <- names(x[[i]])
        asc.cap2 <- ascii(list(NULL), caption = captions2[j], caption.level = caption.level2)
        xx[[paste("obj", i, j, sep = "")]] <- asc.cap2
        for (k in 1:length(x[[i]][[j]])) {
          xx[[paste("obj", i, j, k, sep = "")]] <- ascii(x[[i]][[j]][[k]], caption = captions3[k], caption.level = caption.level3, format = format, digits = digits)
        }
      }
    }
  }
  xx
}

print.remix <- function(x, type = "rest", caption.level = 1:3, ...) {
  print(ascii(x, caption.level = caption.level, ...), type = "rest")
  invisible(x)
}

is.remix <- function(x)
    inherits(x, "remix")
