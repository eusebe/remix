\name{freq}
\alias{freq}
\title{
  Describe a categorical object.
}
\description{
  Describe a categorical object.
}
\usage{
freq(x, useNA = c("no", "ifany", "always"), cum = FALSE) 
}
\arguments{
  \item{x}{
    A character or factor vector.
  }
  \item{useNA}{
    Whether to include extra NA levels in the frequency table.
  }
  \item{cum}{
    Should cumulated frequencies be reported ?
  }
}
\value{
  A freq object, basically a data.frame with descriptive statistics.
}
\author{
  David Hajage
}

\seealso{
  \code{\link{mix}}
}
\examples{
freq(iris$Species)
}
\keyword{univar}

