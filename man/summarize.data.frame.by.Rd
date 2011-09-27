\name{summarize.data.frame.by}
\alias{summarize.data.frame.by}
\title{Compute summary statistics according to a factor (data.frame input)}
\usage{
  summarize.data.frame.by(df, by, funs = c(mean, sd,
  quantile, n, na), ..., useNA = c("no", "ifany",
  "always"), addmargins = FALSE, revert = FALSE, test =
  FALSE, test.summarize = test.summarize.auto, show.test =
  display.test, plim = 4, show.method = TRUE, label =
  FALSE)
}
\arguments{
  \item{df}{data.frame}

  \item{by}{data.frame}

  \item{funs}{fuctions}

  \item{...}{passed to funs}

  \item{useNA}{useNA}

  \item{addmargins}{addmargins}

  \item{revert}{whether to regroup factors or numeric
  variables when crossing factor with numeric variables}
}
\description{
  Compute summary statistics according to a factor
  (data.frame input)
}
\author{
  David Hajage
}
\keyword{internal}

