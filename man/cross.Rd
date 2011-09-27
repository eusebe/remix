\name{cross}
\alias{cross}
\title{Cross x and y}
\usage{
  cross(x, y = NULL, funs = c(mean, sd, quantile, n, na),
  ..., cum = FALSE, margin = 0:2, addmargins = FALSE, useNA
  = c("no", "ifany", "always"), propNA = TRUE, revert =
  FALSE, method = c("pearson", "kendall", "spearman"),
  times = NULL, test = FALSE, test.tabular =
  test.tabular.auto, test.summarize = test.summarize.auto,
  test.survival = test.survival.logrank, show.test =
  display.test, plim = 4, show.method = TRUE, label =
  FALSE)
}
\arguments{
  \item{x}{x}

  \item{y}{y}

  \item{funs}{funs}

  \item{...}{\dots}

  \item{cum}{cum}

  \item{margin}{margin}

  \item{addmargins}{addmargins}

  \item{useNA}{useNA}

  \item{propNA}{propNA}

  \item{revert}{whether to regroup factors or numeric
  variables when crossing factor with numeric variables}

  \item{method}{method}

  \item{test}{test}

  \item{test.tabular}{test.tabular}

  \item{test.summarize}{test.summarize}

  \item{show.test}{show.test}

  \item{plim}{plim}

  \item{show.method}{show.method}

  \item{label}{display label? (using \code{Hmisc:::label})}
}
\description{
  Cross x and y
}
\author{
  David Hajage
}
\keyword{internal}

