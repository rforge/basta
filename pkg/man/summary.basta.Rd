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
\method{plot}{basta}(x, plot.trace = TRUE, trace.name = "theta", \dots)
}
\arguments{
  \item{object }{an object of class '\code{basta}'}
  \item{x }{an object of class '\code{basta}'}
  \item{plot.trace}{logical, if \code{TRUE} the raw parameter traces are plotted, else, the predictive intervals for the resulting survival probability and mortality rates are plotted.}
  \item{trace.name }{character string indicating the set of parameters or posteriors to be plotted. The options are: "theta" to plot the survival model parameters; "gamma" to plot the proportional hazards parameters (if it applies, else \code{plot.basta} returns an error); "pi" to plot the recapture probabilities; and "post" to plot the conditional posteriors for the parameters and for the latent ages at death, and the full posterior for the model.}
  \item{\dots }{additional arguments passed to functions \code{print}, \code{summary} and \code{plot}.}
}
\details{
For objects of class \code{basta}, function \code{print} returns three summary elements describing the model and its results, namely: \code{call}, \code{run} and \code{coefficients}. \code{call} describes the basic model used (i.e. exponential, Gompertz, Weibull or logistic), the shape chosen, \dQuote{\code{simple}}, \dQuote{\code{Makeham}} or \dQuote{\code{bathtub}}, the covariate structure chosen, \dQuote{\code{mixed}}, \dQuote{\code{prop.haz}} or \dQuote{\code{all.in.mort}} and which covariates (if any) were categorical and which continuous. 

The summary element \code{run} describes if all simulations ran for the totality of the \code{niter} chosen by the user on function \code{\link{basta}} or, if not all the runs finished, it outlines which of these failed.

The summary element \code{coefficients} prints out the estimated coefficients for all parameters in the model, as well as their standard errors and 95\% upper and lower credible intervals. It also includes a measure of serial autocorrelation for each parameter calculated from the thinned parameter chains, an update rate per parameter (commonly the same for all survival and proportional hazards parameters), and potential scale reduction factor for each parameter as a measure of convergence (Gelman \emph{et al.} 2004). 

Function \code{summary} includes all the previous elements, as well as a summary description of the priors and jump standard deviations for all survival parameters, a calibration of the Kullback-Liebler discrepancy as a measure of parameter similarities for those parameters associated to categorical covariates (McCulloch 1989), and a measure of model fit based on DICs (Spiegelhalter \emph{et al.} 2002). 

Function \code{plot} takes objects of class \code{basta} to create trace plots or, if the argument for \code{plot.trace} is set to \code{FALSE}, it plots estimated survival probabilities and mortality rates with their 95\% predictive intervals. 
}
\references{
Gelman, A., Carlin, J.B., Stern, H.S. & Rubin, D.B. (2004) \emph{Bayesian data analysis}. 2nd edn. Chapman & Hall/CRC, Boca Raton, Florida, USA.

McCulloch, R.E. (1989) Local model influence. \emph{Journal of the American Statistical Association}, 84, 473-478.

Spiegelhalter, D.J., Best, N.G., Carlin, B.P., Van Der Linde, A. (2002) Bayesian measures of model complexity and fit. Journal of the Royal Statistical Society: Series B 64, 583-639.
}
\author{
Fernando Colchero \email{colchero@demogr.mpg.de}, Owen R. Jones \email{jones@demogr.mpg.de} and Maren Rebke \email{rebke@demogr.mpg.de}
}

\seealso{
\code{\link{basta}}
}

\example{
## Load BaSTA output:
data("basta.out", package = "BaSTA")

## Print summary output:
summary(basta.out)

## Plot traces for mortality parameters (theta):
plot(basta.out)

## Plot traces for proportional hazards parameters (gamma):
plot(basta.out, trace.name = "gamma")

## Plot predicted mortality and survival:
plot(basta.out, plot.trace = FALSE)
}

