\name{summary.basta}
\alias{summary.basta}
\alias{print.basta}
\alias{plot.basta}

\title{
Summarizing and plotting Bayesian Survival Trajectory Analysis (BaSTA) model outputs.
}
\description{
These functions are all methods for class \code{basta}
}
\usage{
## S3 methods for class '\code{basta}'
summary.basta(object, digits=3)
print.basta(object, digits=3)
plot.basta(object, plot.type = "traces", tracename = "theta", ...)
}
\arguments{
  \item{object}{an object of class "\code{basta}"}
  \item{digits}{the number of significant digits to use when printing the coefficients table.}
  \item{plot.type}{a character string indicating the values to be ploted. Two options are available: "traces" to plot the raw parameter traces; or "sxmx" to plot predictive intervals for the resulting survival probability and mortality rates.}
  \item{tracename}{character string indicating the set of parameters or posteriors to be plotted. The options are: "theta" to plot the survival model parameters; "gamma" to plot the porportional hazards parameters (if it applies, else \code{plot.basta} returns an error); "pi" to plot the recapture probabilities; and "post" to plot the conditional posteriors for the parameters and for the latent ages at death, and the full posterior for the model.}
  \item{...}
}
\details{
When more than one categorical variable is evaluated and argument \code{Prop.Hazards} is \code{FALSE}, then
}
\author{
Fernando Colchero \email{colchero@demogr.mpg.de}
}

\seealso{
\code{\link{basta}}
}
\examples{
   print("under construction...")
}

