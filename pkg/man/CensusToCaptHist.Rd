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
Argument \code{d} can be specified as an object of class \code{POSIXct} or \code{POSIXlt}, or as a character string indicating the day, month and year. 
}

\author{
Owen R. Jones \email{jones@demogr.mpg.de}
}

\seealso{
\code{\link{MakeCovMat}} to format a covariate matrix compatible with this output. 
\code{\link{basta}}
}
\examples{
id.vec     <- sort(sample(1:5, size = 15, replace = TRUE))
d.vec      <- rep(0, length(id.vec))
for(i in unique(id.vec)){
  svec     <- which(id.vec == i)
  d.vec[svec] <- sort(sample(1990:1995, length(svec)))
}
Y          <- CensusToCaptHist(ID = id.vec,  d = d.vec, 
                               dformat = "yyyy")
}
