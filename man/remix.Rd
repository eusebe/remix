\name{remix}
\alias{remix}
\title{Remix and describe.}
\usage{
  remix(formula = cbind(...) ~ ., data = NULL, funs =
  c(mean, sd, quantile, n, na), ..., cum = FALSE, margin =
  0:2, addmargins = FALSE, useNA = c("no", "ifany",
  "always"), propNA = TRUE, revert = FALSE, method =
  c("pearson", "kendall", "spearman"), times = NULL,
  followup = FALSE, test = FALSE, test.summarize =
  test.summarize.auto, test.survival =
  test.survival.logrank, test.tabular = test.tabular.auto,
  show.test = display.test, plim = 4, show.method = TRUE,
  label = FALSE)
}
\arguments{
  \item{formula}{a formula (see Details).}

  \item{data}{a data.frame.}

  \item{funs}{functions for describing numeric variable.
  Can be \code{c(fun1, fun2, fun3)} or \code{c("fun1",
  "fun2", "fun3")} or a list.}

  \item{...}{further arguments (all passed to funs), for
  example {na.rm = TRUE}}

  \item{cum}{should cumulated frequencies be reported?}

  \item{margin}{index, or vector of indices to generate
  proportion in frequency tables (0: cell, 1: row, 2:
  col).}

  \item{addmargins}{whether to add margins}

  \item{useNA}{whether to include NA as a level (factor)}

  \item{propNA}{whether to include NA in proportion
  calculation}

  \item{revert}{whether to regroup factors or numeric
  variables when crossing factor with numeric variables}

  \item{method}{a character string indicating which
  correlation coefficient is to be used. One of
  \code{"pearson"}, \code{"kendall"}, or \code{"spearman"},
  can be abbreviated.}

  \item{times}{vector of times (see
  \code{?summary.survival} un package \code{survival})}

  \item{followup}{whether to display follow-up time}

  \item{test}{whether to perform tests}

  \item{test.summarize}{a function of two arguments
  (continuous variable and grouping variable) used to
  compare continuous variable, that return a list of two
  components : \code{p.value} and \code{method} (the test
  name). See \code{test.summarize.auto},
  \code{test.summarize.kruskal},
  \code{test.summarize.oneway.equalvar}, or
  \code{test.summarize.unequalvar} for example of such
  functions. Users can provide their own function.}

  \item{test.survival}{a function of one argument (a
  formula) used to compare survival estimations, that
  returns the same components as created by
  \code{test.summarize}. See \code{test.survival.logrank}.
  Users can provide their own function.}

  \item{test.tabular}{a function of three arguments (two
  categorical variables and a logical \code{na}) used to
  test association between two factors, that returns the
  same components as created by \code{test.summarize}. See
  \code{test.tabular.auto} and \code{test.tabular.fisher}.
  Users can provide their own function.}

  \item{show.test}{a function used to display the test
  result. See \code{display.test}.}

  \item{plim}{number of digits for the p value}

  \item{show.method}{should display the test name?}

  \item{label}{whether to display labels of variables
  (using \code{label} in package \code{Hmisc})}
}
\value{
  A remix object, basically a list with descriptive tables.
  It uses \code{ascii} package for printing output, and can
  be use with \code{ascii} function.
}
\description{
  A quick and easy function for describing datasets.
}
\note{
  The formula has the following format: \code{x_1 + x_2 +
  ... ~ y_1 + y_2 + ...}

  There are a couple of special variables: \code{...}
  represents all other variables not used in the formula
  and \code{.} represents no variable, so you can do
  \code{formula = var1 ~ .}.

  If \code{var1} is numeric, \code{var1 ~ .} produce a
  summary table using \code{funs}. If \code{var1} is a
  factor, \code{var1 ~ .} produce a frequency table. If
  \code{var1} is of class \code{Surv}, \code{var1 ~ .}
  produce a table with the estimates of survival at
  \code{times}. If \code{var1} is numeric and \code{var2}
  is numeric, \code{var1 ~ var2} gives correlation. if
  \code{var1} is numeric and \code{var2} is a factor,
  \code{var1 ~ var2} produce a summary table using
  \code{funs} according to the levels of \code{var2}. If
  \code{var1} is a factor and \code{var2} is a factor,
  \code{var1 ~ var2} produce a contingency table. If
  \code{var1} is of class \code{Surv} and \code{var2} is a
  factor, \code{var1 ~ var2} produce a table with the
  estimates of survival for each level of \code{var2}.

  You can group several variables of the same type (numeric
  or factor) together with \code{cbind(var1, var2, var3)},
  they will be grouped in the same table. \code{cbind(...)}
  works (ie regroups all variables of the same type).
}
\examples{
parwidth <- getOption("width")
options(width = 100)

library(remix)
remix(data = iris)
remix(cbind(...) ~ ., iris[, sapply(iris, is.numeric)], funs = c(median, mad, min, max))
remix(cbind(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs = quantile, probs = c(1/3, 2/3))
remix(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris)
remix(cbind(Sepal.Length, Sepal.Width) ~ cbind(Petal.Length, Petal.Width), iris)
remix(... ~ ., esoph, cum = TRUE)
remix(alcgp ~ tobgp, esoph, cum = TRUE)
remix(Surv(time, status) ~ x, data = aml, times = seq(0, 120, 12))

options(width = parwidth)
}
\author{
  David Hajage, inspired by the design and the code of
  \code{summary.formula} (\code{Hmisc} package, FE Harrell)
  and \code{cast} (\code{reshape} package, H Wickham).
}
\seealso{
  \code{cast} (reshape) and \code{summary.formula} (Hmisc).
}
\keyword{univar}

