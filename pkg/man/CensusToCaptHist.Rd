\name{CensusToCaptHist}
\alias{CensusToCaptHist}

\title{
Construct capture-history matrix from census matrix to be used in BaSTA.
}
\description{
This function takes a common census table and converts it into a capture-history matrix to be used as part of a BaSTA data input.
}
\usage{
CensusToCaptHist(ID, d, dformat = "dd/mm/yyyy")
}

\arguments{
  \item{ID}{
vector with individual ids.
}
  \item{d}{
vector of dates when each individual was observed
}
  \item{dformat}{
format for \code{d}
}
}
\details{
under construction...
}

\author{
Owen R. Jones \email{jones@demogr.mpg.de}
}

\seealso{
\code{\link{makecovm}} to format a covariate matrix compatible with this output. 
\code{\link{basta}}
}
\examples{
##under construction
}
