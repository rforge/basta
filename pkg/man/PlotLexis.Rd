\name{PlotLexis}
\alias{PlotLexis}

\title{
Function to plot a Lexis diagram from a Bayesian Survival Trajectory Analysis (BaSTA) analysis object.
}
\description{
This function produces a Lexis diagram from the predicted birth and death times within a basta object.}
\usage{
PlotLexis(x, ci = 1, ji = 0.5, ...)
}

\arguments{
 
 \item{x }{An object of class \code{\link{basta}}.}
 
 \item{ci }{A data frame of n rows (n = number of individuals in dataset) and nz columns (number of general covariates) including categorical covariates as factors (e.g. sex with individual labels "f","m", location, etc.) and/or continuous individual covariates (e.g. weight at birth, general weather conditions at each location, etc.).}

\item{ji }{A jitter value, to separate the lines when there are many individuals with the same birth times.}

\item{... }{Additional plotting commands such as \code{col}, \code{xlab}, \code{ylab}, etc.}

}

\details{}

\value{ }

\author{
Owen R. Jones \email{jones@demogr.mpg.de}
}

\seealso{
\code{\link{basta}}
}
\examples{
data("sim1Out", package = "BaSTA")
PlotLexis(sim1Out)
}
