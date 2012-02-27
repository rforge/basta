\name{PlotLexis}
\alias{PlotLexis}

\title{
Function to plot a Lexis diagram from a Bayesian Survival Trajectory Analysis (BaSTA) analysis object.
}
\description{
This function produces a Lexis diagram from the predicted birth and death times within a basta object.}
\usage{
PlotLexis(x, ji = 0.5, trans=50, q = 0.5, lw = 2, ...)
}

\arguments{
 
\item{x }{An object of class \code{\link{basta}}.}

\item{ji }{A jitter value, to separate the lines when there are many individuals with the same birth times.}

\item{trans }{The level of transparency to be used when plotting the lines for each individual. A value of 'NULL' plots lines with no transparency.}
 
\item{q }{Defines the quantile to be plotted. The default value is 0.5, to plot the 50% quantile}

\item{lw }{Line width for the plot.}

\item{... }{Additional plotting commands which can be passed to plot. e.g. \code{xlab}, \code{ylab}, etc.}

}

\author{
Owen R. Jones \email{jones@demogr.mpg.de}
}

\seealso{
\code{\link{basta}}
}
\examples{
data("sim1Out", package = "BaSTA")
PlotLexis(sim1Out,xlab= "Year", ylab = "Age (yrs)")
}
