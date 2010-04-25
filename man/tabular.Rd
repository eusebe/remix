\name{tabular}
\alias{tabular}
\title{
  Describe a categorical object.
}
\description{
  Describe a categorical object.
}
\usage{
tabular(x, y, margin = 0:2, useNA = c("no", "ifany", "always")) 
}
\arguments{
  \item{x, y}{
    A character or factor vector.
  }
  \item{margin}{
    Index, or vector of indices to generate proportion in frequency
    tables (0: cell, 1: row, 2: col).
  }
  \item{useNA}{
    Whether to include extra NA levels in the table.
  }
}
\value{
  A tabular object, basically a data.frame with descriptive statistics.
}
\author{
  David Hajage
}

\seealso{
  \code{\link{mix}}
}
\examples{
tabular(esoph$alcgp, esoph$tobgp)
}
\keyword{univar}

