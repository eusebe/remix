\name{correlation}
\alias{correlation}
\title{
  Compute correlation.
}
\description{
  Compute correlation between two numerical objects.
}
\usage{
correlation(x, y, method = c("pearson", "kendall", "spearman"))
}
\arguments{
  \item{x, y}{
    A character or factor vector.
  }
  \item{method}{
    A character string indicating which correlation coefficient is to be
    used. One of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}, 
    can be abbreviated.
  }
}
\value{
  A correlation object, basically a vector with descriptive statistics.
}
\author{
  David Hajage
}

\seealso{
  \code{\link{mix}}
}
\examples{
correlation(iris$Sepal.Length, iris$Petal.Length)
}
\keyword{univar}

