\name{makecovm}
\alias{makecovm}

\title{
Function to build a matrix of covariates (i.e. design matrix) for a BaSTA analysis.
}
\description{
This function creates a matrix of covariates or design matrix appropriate for BaSTA from raw individual level covariate data. The function identifies categorical and continuous covariates and organizes them accordingly. 
}
\usage{
makecovm(Data, covs)
}

\arguments{
  \item{Data}{
a matrix of n rows (n = number of individuals in dataset) and nz columns (number of general covariates) including categorical covariates as factors (e.g. sex with individual labels "f","m", or "u" for unknowns, location, etc.) and/or continuous individual covariates (e.g. weight at birth, general weather conditions at corresponding location, etc.).
}
  \item{covs}{
the column names in the \code{Data} matrix to be included in the analysis.
}
}
\details{
under construction
}
\value{
The function returns a new covariate matrix to be collated to a matrix that includes a column for individual ids, a column for time of birth and on for times of death, and the full recapture matrix.
}

\author{
Owen R. Jones \email{jones@demogr.mpg.de}
}

\seealso{
\code{\link{basta}}
}
\examples{
##under construction
}
