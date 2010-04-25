freq <- function(x, useNA = c("no", "ifany", "always"), cum = FALSE) {
  rnames <- as.character(as.list(substitute(list(x)))[-1])
  
  n <- table(x, useNA = useNA)
  p <- prop.table(n)
  if (cum) {
    n.cum <- cumsum(n)
    p.cum <- cumsum(p)
  } else {
    n.cum <- NULL
    p.cum <- NULL
  }
  results <- as.data.frame(cbind(n, p, n.cum, p.cum))
  attr(results, "lgroup") <- rnames
  attr(results, "n.lgroup") <- nrow(results)
  class(results) <- c("freq", "data.frame")
  results
}

freq.data.frame <- function(df, useNA = c("no", "ifany", "always"), cum = FALSE) {
  dfl <- as.list(df)
  rnames <- names(dfl)
  results <- lapply(dfl, freq, useNA = useNA, cum = cum)
  nrows <- sapply(results, nrow)
  nxx <- rep(rnames, nrows)
  results <- rbind.list(results)
  rnamesrep <- paste(nxx, rownames(results), sep = ": ")
  results <- data.frame(results, row.names = rnamesrep)
  class(results) <- c("data.frame", "freq")
  attr(results, "lgroup") <- rnames
  attr(results, "n.lgroup") <- nrows
  class(results) <- c("freq", "data.frame")
  results
}

ascii.freq <- function(x, format = "g", digits = 5, ...) {
  lgroup <- c(attr(x, "lgroup"))
  n.lgroup <- c(attr(x, "n.lgroup"))
  rownames <- sub("(.+)(\\:)(.+)", "\\3", rownames(x))
  class(x) <- class(x)[-1]
  ascii:::ascii(x, include.colnames = TRUE, include.rownames = TRUE, rownames = rownames, lgroup = lgroup, n.lgroup = n.lgroup, format = format, digits = digits, ...)
}

print.freq <- function(x, type = "rest", ...) {
  print(ascii(x, ...), type = type)
  invisible(x)
}

is.freq <- function(x)
  inherits(x, "freq")

prop.table2 <- function (x, margin = 0) {
  # 0 : cell
    if (margin != 0) 
        sweep(x, margin, margin.table(x, margin), "/", check.margin = FALSE)
    else x/sum(x)
}

tabular <- function(x, y, margin = 0:2, useNA = c("no", "ifany", "always")) {
  n <- table(x, y, useNA = useNA)
  p <- mapply(prop.table2, list(n), margin, SIMPLIFY = FALSE)
  names(p) <- sapply(as.character(margin), function(x) switch(x, "0" = "cell", "1" = "row", "2" = "col"))
  results <- c(n = list(n), p)
  results <- data.frame(do.call(ascii:::interleave.matrix, results), check.names = FALSE)
  attr(results, "lgroup") <- list(rep(c("n", names(p)), nrow(n)), rownames(n))
  attr(results, "n.lgroup") <- list(1, 1+length(p))
  attr(results, "tgroup") <- NULL
  attr(results, "n.tgroup") <- NULL
  class(results) <- c("tabular", "data.frame")
  results
}

tabular.data.frame <- function(dfx, dfy, margin = 0:2, useNA = c("no", "ifany", "always")) {
  results <- lapply(dfy, function(y) lapply(dfx, tabular, y, margin = margin, useNA = useNA))
  
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
  
  class(results) <- c("tabular", "data.frame")
  results
}

ascii.tabular <- function(x, format = "g", digits = 5, ...) {
  class(x) <- class(x)[-1]
  ascii:::ascii(x, lgroup = attr(x, "lgroup"), n.lgroup = attr(x, "n.lgroup"), tgroup = attr(x, "tgroup"), n.tgroup = attr(x, "n.tgroup"), include.colnames = TRUE, header = TRUE, format = format, digits = digits, ...)
}

print.tabular <- function(x, type = "rest", ...) {
  print(ascii:::ascii(x, ...), type = type)
  invisible(x)
}

is.tabular <- function(x)
  inherits(x, "tabular")
