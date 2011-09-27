\name{summarize.by}
\alias{summarize.by}
\title{Compute summary statistics according to a factor}
\usage{
  summarize.by(x, by, funs = c(mean, sd, quantile, n, na),
  ..., useNA = c("no", "ifany", "always"), addmargins =
  FALSE)
}
\arguments{
  \item{x}{numeric object}

  \item{by}{factor}

  \item{funs}{functions}

  \item{...}{passed to funs}

  \item{useNA}{useNA}

  \item{addmargins}{addmargins}
}
\description{
  Compute summary statistics according to a factor
}
\author{
  David Hajage
}
\keyword{internal}

