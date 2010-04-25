\name{resume}
\alias{resume}
\title{
  Resume a numerical object.
}
\description{
  Resume a numerical object.
}
\usage{
resume(x, funs = c(mean, sd, quantile), ...) 
}
\arguments{
  \item{x}{
    A numerical vector.
  }
  \item{funs}{
    Functions. Can be \code{c(fun1, fun2, fun3)} 
    or \code{c("fun1", "fun2", "fun3")} or a list.
  }
  \item{\dots}{
    Further arguments (all passed to funs), for example \code{na.rm = TRUE}\dots.
  }
}
\value{
  A resume object, basically a vector with descriptive statistics.
}
\author{
  David Hajage
}

\seealso{
  \code{\link{mix}}
}
\examples{
resume(iris$Sepal.Length)
}
\keyword{univar}

