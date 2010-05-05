##' Remove blank in a formula
##'
##' @param formula formula (character)
##' @author David Hajage
##' @keywords internal
remove_blank <- function(formula) {
  gsub(" ", "", formula)
}

##' Separate left, right and by part of a formula
##'
##' @param formula formula (character)
##' @author David Hajage
##' @keywords internal
left_right <- function(formula) {
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }
  
  left <- formula[[2]]
  if (length(left) == 1) {
    left <- deparse(left)
  } else {
    left <- attr(terms(formula(paste("~", deparse(left))), allowDotAsName = TRUE), "term.labels")
  }
  right <- formula[[3]]
  if (length(right) == 1) {
    right <- deparse(right)
    by <- "."
  } else {
    if (right[[1]] == "|") {
      by <- right[[3]]
      if (length(by) == 1) {
        by <- deparse(by)
      } else {
        by <- attr(terms(formula(paste("~", deparse(by))), allowDotAsName = TRUE), "term.labels")
        by <- by[by != "."]
      }
      right <- right[[2]]
    } else {
      by <- "."
    }
    right <- attr(terms(formula(paste("~", deparse(right))), allowDotAsName = TRUE), "term.labels")
  }
  return(list(left = left, right = right, by = by))
}

##' Check if a variable is repeated several times in a formula
##'
##' @param formula formula (character)
##' @author David Hajage
##' @keywords internal
check_formula <- function (formula) {
  lr <- left_right(formula)[c("left", "right")]
  if (any(lr$left == ".") & any(lr$right == "."))
    stop("You can't cross nothing with nothing (. ~ .)")
  vars <- unlist(lr)
  if (length(unique(vars)) < length(vars)) 
    warning("Variable(s) repeated several times: ", paste(names(table(vars))[table(vars) > 1], collapse = ", "), call. = FALSE)
}

##' Expand ... in a formula
##'
##' @param formula formula (character)
##' @param varnames variables names
##' @author David Hajage
##' @keywords internal
expand_formula <- function(formula, varnames) {
  formula <- remove_blank(formula)
  vars <- all.vars(as.formula(formula))
  collapse <- "+"
  if (grepl("cbind *\\(.*\\.\\.\\..*\\)", formula))
    collapse <- ","
  replacement <- paste(setdiff(varnames, vars), collapse = collapse)
  formula <- sub("\\.\\.\\.", replacement, formula)
  as.character(formula)
}

##' Parse a formula
##'
##' @param formula formula (character)
##' @param varnames variables names
##' @author David Hajage
##' @keywords internal
parse_formula <- function(formula, varnames) {
  check_formula(formula)
  formula <- expand_formula(formula, varnames)
  left_right(formula)
}

##' Parse data
##'
##' @param formula formula (character)
##' @param data data
##' @author David Hajage
##' @keywords internal
parse_data <- function(formula, data) {
  vars <- unlist(left_right(formula))
  vars <- vars[vars != "."]
  vars <- gsub("(cbind *\\()(.*)(\\))", "\\2", vars)
  vars <- unlist(strsplit(gsub("(cbind *\\()(.*)(\\))", "\\2", vars), ","))
  formula <- paste("~", paste(vars, collapse = "+"), sep = "")
    
  results <- model.frame(formula, data, na.action = NULL)
  inter <- unlist(strsplit(formula, "\\~|\\+"))
  inter <- inter[grep(":", inter)]
  varinter <- strsplit(inter, ":")
  dfinter <- as.data.frame(lapply(varinter, function(x) interaction(results[, x])))
  names(dfinter) <- inter
  if (!all(dim(dfinter) == 0))
    results <- data.frame(results, dfinter, check.names = FALSE)
  results
}
