\name{DataCheck}
\alias{DataCheck}
\title{A function to check the input data file for a BaSTA analysis.}

\description{This function performs some basic error checking on the input files for a BaSTA analysis. A number of checks are performed including (1) individuals dying before the observation window starts; (2) birth date recorded as being after death date; (3) no observations of any kind (phantom individuals); (4) observations before birth; (5) observations after death; (6) years of birth and deaths must appear as 0 in the observation matrix.}

\usage{DataCheck(object, ststart, stend, autofix = rep(0, 7), silent = TRUE)}

\arguments{
  \item{object }{A data.frame to be used as an input data file for BaSTA. The first column is the individual's ID, the second and third columns are birth and death years respectively. Columns 4 to (nt+3) represent the observation window of nt years. This is followed (optionally) by columns for covariate.}
  \item{ststart }{The start year of the observation window.}
  \item{stend }{The end year of the observation window.}
  \item{autofix }{A logical argument indicating whether to automatically fix any errors. This should be used with extreme caution. We recommend going back to the individual-based data and fixing each error "by hand".}
  \item{silent }{A logical argument indicating whether to print a detailed report to the screen or not.}
}

\details{
Further details of the autofix argument:

}

\value{
  \item{ok }{A logical indicator that indicates if the data are free of errors or not. i.e. TRUE = the data are ok.}
  \item{newData }{A corrected data frame.}
  \item{type0 }{A vector of row numbers in the original data frame where there are deaths before the observation window starts.}
  \item{type1 }{A vector of row numbers in the original data frame where there are deaths occurring before the study starts.}
  \item{type2 }{A vector of row numbers in the original data frame where there are no birth/death AND no obervations.}
  \item{type3 }{A vector of row numbers in the original data frame where there are births recorded after death.}
  \item{type4 }{A vector of row numbers in the original data frame where there are observations (i.e. recaptures) after death.}
  \item{type5 }{A vector of row numbers in the original data frame where there are observations (i.e. recaptures) before birth.}
  \item{type6 }{A vector of row numbers in the original data frame where the year of birth is not a zero in the recapture matrix.}
  \item{type7 }{A vector of row numbers in the original data frame where the year of death is not a zero in the recapture matrix.}
}

\author{Owen R. Jones \email{jones@demogr.mpg.de},  Maren Rebke \email{rebke@demogr.mpg.de} and Fernando Colchero \email{colchero@demogr.mpg.de}}

\seealso{\code{\link{basta}}}

\examples{
## under construction
}

