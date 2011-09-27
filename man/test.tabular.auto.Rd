\name{test.tabular.auto}
\alias{test.tabular.auto}
\title{test for contingency table}
\usage{
  test.tabular.auto(x, y, na = FALSE)
}
\arguments{
  \item{x}{vector}

  \item{y}{another vector}

  \item{na}{use NA ?}
}
\value{
  a list with two componments: p.value and method
}
\description{
  Compute a chisq.test, a chisq.test with correction of
  continuity or a fisher test as appropriate
}
\author{
  David Hajage
}

