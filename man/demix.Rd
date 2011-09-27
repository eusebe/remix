\name{demix}
\alias{demix}
\title{Demix}
\usage{
  demix(x)
}
\arguments{
  \item{x}{a remix object}
}
\value{
  A list of data.frame.
}
\description{
  Transfrom a remix object into a (list of) data.frame(s).
}
\examples{
x <- remix(... ~ ., esoph, cum = TRUE)
demix(x)
}
\author{
  David Hajage
}
\seealso{
  \code{remix}
}

