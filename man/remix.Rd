\name{remix}
\alias{remix}
\title{
Remix and describe.
}
\description{
  A quick and easy function for describing datasets.
}
\usage{
remix(formula, data, funs = c(mean, sd, quantile), ..., cum = FALSE,
margin = 0:2, useNA = "no", method = c("pearson", "kendall",
"spearman"))
}
\arguments{
  \item{formula}{
    A formula (see Details).
}
  \item{data}{
    A data.frame.
}
  \item{funs}{
    Functions for describing numeric variable. Can be \code{c(fun1, fun2, fun3)} 
    or \code{c("fun1", "fun2", "fun3")} or a list.
}
  \item{\dots}{
    Further arguments (all passed to funs), for example \code{na.rm = TRUE}\dots.
}
  \item{cum}{
    Should cumulated frequencies be reported ?
}
  \item{margin}{
    Index, or vector of indices to generate proportion in frequency
    tables (0: cell, 1: row, 2: col).
}
  \item{useNA}{
    Whether to include extra NA levels in the frequency tables.
}
  \item{method}{
    A character string indicating which correlation coefficient is to be
    used. One of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}, 
    can be abbreviated.
}
}
\details{
  The formula has the following format: \code{x_1 + x_2 + ... ~ y_1 +
    y_2 + ... | z_1 + z_2 + ...}

  There are a couple of special variables: \code{...} represents all
  other variables not used in the formula and "." represents no
  variable, so you can do \code{formula = var1 ~ .}.

  You can group several variables of the same type (numeric or factor)
  together with \code{c(var1, var2, var3)}, they will be grouped in the
  same table. \code{c(...)} works (but only if all variables are of the
  same type).

  You can exclude one or more variables from the formula with \code{-}.
  For example: \code{... - var1 - var2} remove \code{var1} and
  \code{var2} from the formula.
}

\value{
  A remix object, basically a list with descriptive tables.
}

\author{ David Hajage }

\note{
  This package is inspired by the design and the code of the \code{cast}
  function from reshape package and the \code{summary.formula} function
  from Hmisc package.
}

\seealso{
  cast (reshape) and summary.formula (Hmisc).
}

\examples{
parwidth <- getOption("width")
options(width = 100)

remix(... ~ ., iris)
remix(c(...) ~ ., iris[, sapply(iris, is.numeric)], funs = c(median, mad, min, max))
remix(c(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs = quantile, probs = c(1/3, 2/3))
remix(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris)
remix(c(Sepal.Length, Sepal.Width) ~ c(Petal.Length, Petal.Width), iris)
remix(c(Sepal.Length, Sepal.Width) ~ c(Petal.Length, Petal.Width) | Species, iris)
remix(... ~ ., esoph, cum = TRUE)
remix(alcgp ~ tobgp, esoph, cum = TRUE)
remix(agegp ~ . | alcgp + tobgp, esoph)
remix(agegp ~ . | alcgp:tobgp, esoph)

options(width = parwidth)
}

\keyword{univar}
