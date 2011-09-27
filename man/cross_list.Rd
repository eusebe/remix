\name{cross_list}
\alias{cross_list}
\title{Cross variables in a list}
\usage{
  cross_list(l, funs = c(mean, sd, quantile, n, na), ...,
  cum = FALSE, margin = 0:2, addmargins = FALSE, useNA =
  c("no", "ifany", "always"), propNA = TRUE, revert =
  FALSE, method = c("pearson", "kendall", "spearman"),
  times = NULL, test = FALSE, test.summarize =
  test.summarize.auto, test.survival =
  test.survival.logrank, test.tabular = test.tabular.auto,
  show.test = display.test, plim = 4, show.method = TRUE,
  label = FALSE)
}
\arguments{
  \item{l}{}

  \item{funs}{funs}

  \item{...}{\dots}

  \item{cum}{cum}

  \item{margin}{margin}

  \item{addmargins}{addmargins}

  \item{useNA}{useNA}

  \item{propNA}{}

  \item{revert}{whether to regroup factors or numeric
  variables when crossing factor with numeric variables}

  \item{method}{method}

  \item{test}{test}

  \item{test.summarize}{test.summarize}

  \item{test.tabular}{test.tabular}

  \item{show.test}{show.test}

  \item{plim}{plim}

  \item{show.method}{show.method}

  \item{label}{label}
}
\description{
  Cross variables in a list
}
\author{
  David Hajage
}
\keyword{internal}

