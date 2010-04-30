##' Remove blank in a formula
##'
##' @param formula formula (character)
remove_blank <- function(formula) {
  gsub(" ", "", formula)
}

# Remove a variable in a formula (not used ?)
#
# @param formula formula (character)
# remove_var <- function(formula) {
#   formula <- remove_blank(formula)
#   gsub("(-.*)([\\+\\~]?)", "\\2", formula)
# }

##' Separate left and right part of a formula
##'
##' @param formula formula (character)
left_right<- function(formula) {
  formula <- remove_blank(formula)
  lr <- unlist(strsplit(formula, "~"))
  lr <- c(lr[1], unlist(strsplit(lr[2], "\\|")))
  lr <- strsplit(lr, "\\+")
  lr <- lapply(lr, function(x) {
    y <- strsplit(x, "-")
    unlist(lapply(y, function(y) y[1]))
  })
  if (length(lr) == 2)
    lr <- c(lr, ".")
    
  names(lr) <- c("left", "right", "by")
  if (all(lr$by == "."))
    lr$by <- "."
  if (length(lr$by) > 1)
    lr$by <- lr$by[lr$by != "."]
  lr$by <- unlist(strsplit(gsub("(cbind *\\()(.*)(\\))", "\\2", lr$by), ","))

  lr
}

##' Check if a variable is repeated several times in a formula
##'
##' @param formula formula (character)
check_formula <- function (formula) {
  vars <- unlist(left_right(formula)[c("left", "right")])
  if (length(unique(vars)) < length(vars)) 
    warning("Variable(s) repeated several times: ", paste(names(table(vars))[table(vars) > 1], collapse = ", "), call. = FALSE)
}

##' Expand ... in a formula
##'
##' @param formula formula (character)
##' @param varnames variables names
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
parse_formula <- function(formula, varnames) {
  check_formula(formula)
  formula <- expand_formula(formula, varnames)
  left_right(formula)
}

##' Parse data
##'
##' @param formula formula (character)
##' @param data data
parse_data <- function(formula, data) {
  vars <- unlist(left_right(formula))
  vars <- vars[vars != "."]
  vars <- gsub("(cbind *\\()(.*)(\\))", "\\2", vars)
  vars <- unlist(strsplit(gsub("(cbind *\\()(.*)(\\))", "\\2", vars), ","))
  ## vars <- gsub("c *\\(", "cbind(", vars) # mais ne donne pas les bons noms aux variables...
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
