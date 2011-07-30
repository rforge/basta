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
\method{summary}{basta}(object, \dots)
\method{print}{basta}(x, \dots)
\method{plot}{basta}(x, plot.type = "traces", tracename = "theta", \dots)
}
\arguments{
  \item{object}{an object of class '\code{basta}'}
  \item{x}{an object of class '\code{basta}'}
  \item{plot.type}{a character string indicating the values to be ploted. Two options are available: "traces" to plot the raw parameter traces; or "sxmx" to plot predictive intervals for the resulting survival probability and mortality rates.}
  \item{tracename}{character string indicating the set of parameters or posteriors to be plotted. The options are: "theta" to plot the survival model parameters; "gamma" to plot the porportional hazards parameters (if it applies, else \code{plot.basta} returns an error); "pi" to plot the recapture probabilities; and "post" to plot the conditional posteriors for the parameters and for the latent ages at death, and the full posterior for the model.}
  \item{\dots}{additional arguments passed to functions \code{print}, \code{summary} and \code{plot}.}
}
\details{
For objects of class \code{basta}, function \code{print} prints three summaries describing the model and its results, namely: \code{call}, \code{run} and \code{coefficients}. \code{call} describes the model used (i.e. Gompertz, Gompertz-Makeham, or Siler), if covariates were evaluated under a proportional hazards framework, and which covariates (if any) were categorical and which continuous. 

The summary \code{run} describes if all simulations ran for the totality of the \code{niter} chosen by the user on function \code{\link{basta}} or, in case not all runs finished, outlines which runs failed.

The summary \code{coefficients} prints out the estimated coefficients for all parameters in the model, as well as their standard erros and 95\% upper and lower credible intervals. It also includes a measure of serial autocorrelation for each parameter calculated at the thinning intervals specified by the user on function \code{\link{basta}}, an update rate per parameter (commonly the same for all survival and proportional hazards parameters). Finally, potential scale reduction factor for each parameter is depicted as a measure of convergence (Gelman \emph{et al.} 2004). 

Function \code{summary} includes all the previous measures, but includes a summary description of the priors for all survival parameters as well as a measure of model selection based on DICs (Spiegelhalter \emph{et al.} 2002). 

Function \code{plot} takes objects of class \code{basta} to create trace plots or, if the argument for \code{plot.type} is set to \code{sumo}, plots estimated survival probability and mortality rate plots with their 95\% predictive intervals. 
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

