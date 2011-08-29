\name{basta}
\alias{basta}
\alias{basta.default}
\title{
Parametric Bayesian estimation of age-specific survival for truncated and censored capture-recapture/recovery records.
}
\description{
This function performs multiple Monte Carlo Markov Chain (MCMC) algorithms for Bayesian estimation of age-specific mortality and survival trends when a large proportion of (or all) records have unknown times of birth and/or death. Survival parameters and unknown (i.e. latent) birth and death times are estimated, with a choice of three mortality functions (i.e. Gompertz, Gompertz-Makeham and Siler), based on the model described by Colchero & Clark (2011). Categorical and continuous covariates can be included either within a proportional hazards framework or following Colchero and Clark's (2011) approach.}
\usage{
basta(object, \dots)

\method{basta}{default}(object, ststart, stend, model = "GO", Shape="simple",
      niter = 50000, burnin = 5001, thinning = 50, rptp = ststart,
      th.ini.pars = NULL, th.jumps = NULL, th.priors = NULL, 
      Prop.Hazards = FALSE, ga.ini.pars = NULL, ga.jumps = NULL, 
      ga.priors = NULL, nsim = 1, parallel = FALSE, ncpus = 2,
      lifetable=TRUE, progr.plot = FALSE, \dots)
}
\arguments{
  \item{object }{a \code{data.frame} to be used as an input data file for BaSTA. The first is a column of individual unique ID's, the second and third columns are birth and death years respectively. Columns 4-(nt-1) represent the observation window of nt years. This is followed (optionally) by columns for categorical and continous covariates (see details). }
  \item{ststart }{the first year of the study.}
  \item{stend }{the last year of the study.}
  \item{model }{the underlying mortality model to be used. "EX" = exponential, GO"= Gompertz, "WE" = Weibull and "LO" = logistic (see \code{details}).}
  \item{Shape }{The overall shape of the model. Values are: \code{simple} = no extra parameters added; \code{Makeham} = a constant parameter is added to the mortality rate; and \code{bathtub} = a Gompertz declining mortality for early ages and a constant parameter are added to the mortality model.}
  \item{niter }{the total number of MCMC steps.}
  \item{burnin }{number of iterations for the burn in (see details).}
  \item{thinning }{number of skipped MCMC steps to minimize serial autocorrelation (see details).}
  \item{rptp }{a vector (of length npi) defining the recapture probability transition times. These are points (years) where the recapture probability is thought to change.  The default setting is for the recapture probability to be constant throughout the study, so the \code{rptp} is simply defined as a single element vector of the first year of the observation period (e.g. c(1985)). If recapture probabilities were known to change at year say, 1990, the RPTP should be defined as c(1985, 1990).}
  \item{th.ini.pars }{a vector defining the initial values for each parameter on the survival model. The number of parameters should be: 2 for Gompertz, three for Gompertz-Makeham and 5 for Siler (see details). The default is set to \code{NULL} and thus a set of random parameters is generated for each simulation.}
  \item{th.jumps }{a vector defining the size of jumps for each survival model parameter (standard error in the Metropolis step). The number of parameters will depend on the model selected.}
  \item{th.priors }{a vector defining the priors for each survival parameter. The vector takes the same format as described for \code{th.jumps}.}
  \item{Prop.Hazards }{a logical argument that indicates if covariates should be evaluated under a proportional hazards framework. Default is \code{FALSE}. However, if continuous covariates are included in the model, those will be evaluated using a proportional hazards framework (see details).}
  \item{ga.ini.pars }{a vector of initial parameters for the proportional hazards section of the model. These can be specified when Prop.Hazards is TRUE or when continuous covariates are evaluated (see details). As with \code{th.ini.pars}, the default is set to \code{NULL} and thus default values are assigned (i.e. all equal to 0).}
  \item{ga.jumps }{a vector of jump standard errors for the proportional hazards parameters. The number of jumps will be equal to the number of covariates when the argument \code{Prop.Hazards} is set to TRUE, or to the number of continuous covariates to be evaluated. In case any of these two conditions are met and the default is used (i.e. NULL), the model automatically assigns builtin jump values.}
  \item{ga.priors }{a vector of mean priors for the proportional hazards parameters. The specification follows the same rules as \code{cjumps}.}
  \item{nsim }{a numerical value for the number of simulations.}
  \item{parallel }{logical; if TRUE package \pkg{snowfall} is called and multiple simulations are run in parallel. If \pkg{snowfall} is not installed, the model is ran in series.}
  \item{ncpus }{a numerical value that indicates the number of cpus to be used if \code{parallel} is TRUE and package \pkg{snowfall} is installed. The default is 2 cpus. If package pkg{snowfall} is not installed, the simulations are run in series.}
  \item{lifetable }{Logical, if \code{TRUE} a cohort life table is calculated using function \code{\link{CohortLT}}.}
  \item{progr.plot }{logical, if \code{TRUE} small plots are displayed showing the percent progress achieved for each MCMC simulation. }
  \item{\dots }{ignored.}
}
\details{
To construct the input data \code{object} the function \code{\link{CensusToCaptHist}} can be used to build the capture-recapture matrix, while function \code{\link{makecovm}} for the covariates (design) matrix.

The \code{burnin} argument represents the number of steps at the begining of the MCMC run that should be discarded. This sequence commonly corresponds to the non-converged section of the MCMC sequence. Convergence and model selection measures are calculated from the remaining thinned parameter chains.

The argument \code{thinning} specifies the number of steps to be skipped to reduce serial autocorrelation. The thinned sequence, which includes only steps after burnin is then used to calculated convergence statistics and model selection.

The survival (mortality) part of the model includes two main sections: a) the actual survival section, defined by \code{model}; and b) a proportional hazards section, defined by the logical argument \code{Prop.Hazards}. Parameters associated with section a) belong to the generic parameters defined by \eqn{\theta}, while those in section b) belong to parameters \eqn{\Gamma}. The full model is defined as: 

\deqn{\mu(x | \theta, \Gamma, \mathbf{Z}_a, \mathbf{Z}_c) = \mu_0(x | \theta, \mathbf{Z}_a) \, e^{\Gamma \mathbf{Z}_c}}

where \eqn{\mu_0(x | \theta, \mathbf{Z}_a)} represents the survival section a) while the second term \eqn{e^{\Gamma \mathbf{Z}_c}} corresponds to section b) and \eqn{\mathbf{Z}_a} and \eqn{\mathbf{Z}_c} are covariate (design) matrices for categorical and continuous covariates, respectively. If argument \code{Prop.Hazards} is set to \dQuote{FALSE}, these two design matrices are built (given that the two types of covariates are included in \code{object}). If the argument is set to \dQuote{TRUE}, all covariates are included in section b) and only one set of mortality parameters are evaluated in section a). 

At this point, \code{basta} has builtin three survival (mortality) models for section a) as described in Colchero and Clark (2011) (Suplemental Online material S1). The simplest model is Gompertz (\dQuote{\code{GO}}; Gompertz 1925) with two parameters \eqn{\theta = [\alpha, \beta]}, for which the mortality rate is defined as:

\deqn{\mu_0(x | \theta) = e^{\alpha + \beta x}}.
with \eqn{-\infty < \alpha < \infty} and \eqn{-\infty < \beta < \infty}.

The folowing model is Gompertz-Makeham (\dQuote{\code{GM}}) with three parameters \eqn{\theta = [c, \alpha, \beta]}, with mortality rate:

\deqn{\mu_0(x | \theta) = c + e^{\alpha + \beta x}}.
with \eqn{c > -e^{\alpha}} if \eqn{\beta > 0} and \eqn{c > 0} otherwise, \eqn{-\infty < \alpha < \infty} and \eqn{-\infty < \beta < \infty}.

The third model is Siler (\dQuote{\code{SI}}), with five parametes \eqn{\theta = [\alpha_1, \beta_1, c, \alpha_2, \beta_2]}, and with mortality rate:

\deqn{\mu(x | \theta)_0 = e^{\alpha_1 - \beta_1 x} + c + e^{\alpha_2 + \beta_2 x}}.
with \eqn{-\infty < \alpha_1 < \infty} and \eqn{-\infty < \alpha_2 < \infty}, \eqn{\beta_1 > 0} and \eqn{\beta_2 > 0} and \eqn{c > - (e^{\alpha_1 - \beta_1 x_m} + e^{\alpha_2 - \beta_2 x_m})} where \eqn{x_m} is the age at which \eqn{\mu_0(x | \theta)} reaches the mininum vale, and is calculated as:
\deqn{x_m = (\alpha_1+\log(\beta_1) - \alpha_2-\log(\beta_2))/(\beta_1 + \beta_2)}.

The number of parameters in \code{th.ini.pars} is a vector defining the initial values for each parameter on the survival model of section a). The number of parameters will depend on the model chosen with \code{model}(see above). If the number of parameters specified does not match the number of parameters inherent to the model selected, the function returns an error. If no th.pars.mat argument is specified (i.e. default is NULL), the model randomly generates initial parameters.
 
As described above, the number of parameters for argument \code{ga.ini.pars} (i.e. section b), namely the proportional hazards section, will be a function of the number of continuous covariates (\eqn{+ 1} for the intercept) if argument \code{Prop.Hazards} is \dQuote{FALSE}, else the number of parameters needs to be equal to the total number of covariates.
}

\value{
\code{basta} returns an object of class \sQuote{\code{basta}}. Function \code{\link{summary}} can be used to obtain or print a summary of the results.
  \item{coefficients }{Matrix with estimated coefficients (i.e. mean values per parameter on the thinned sequences after burnin), which includes standard errors, upper and lower 95\% credible intervals, update rates per parameter (commonly the same for all survival and proportional hazards parameters), serial autocorrelation on the thinned sequences and potential scale reduction factor for convergence (see \code{Convergence} value below). }
	\item{ModSel }{Basic DIC calculations for model selection (Spiegelhalter \emph{et al.} 2002) These values need to be taken only as a reference (see comments to Spiegelhalter \emph{et al.} 2002). In the future the package will include routines for reversible jump Monte Carlo Markov Chain (RJMCMC, see King & Brooks 2002). If none or some of the simulations did not run through, then the returned value is \dQuote{NULL}.}
	\item{Convergence }{If called by \code{\link{summary}}, matrix with convergence coefficients based on potential scale reduction as described by Gelman \emph{et al.} (2004). If only one simulation was ran, then the returned value is \dQuote{NULL}.}
  \item{settings }{If called by \code{\link{summary}}, vector indicating the number of iterations for each MCMC, the burn in sequence, the thinning interval and the number of simulations ran, as specified by the user.}
	\item{ModelSpecs }{Model specifications inidicating the model and the Shape used as specified by the user.}
	\item{JumpPriors }{If requested or called by functions \code{summary} or \code{\link{summary.basta}}, a matrix with the jumps and priors used in the model.}
	\item{Params }{If requested, the full matrix of parameter estimates. The row names indicate the simulation to which they correspond.}
	\item{Bis }{If requested, the full matrix of estimates of times of birth. The row names indicate the simulation to which they correspond.}
	\item{Dis }{If requested, the full matrix of estimates of times of death. The row names indicate the simulation to which they correspond.}
	\item{Bq }{If requested, summary matrix of estimated times of birth including median and upper and lower 95\% predictive intervals.}
	\item{Xq }{If requested, summary matrix of estimated ages at death including median and upper and lower 95\% predictive intervals.}
	\item{Sx }{If requested or called by functions \code{plot} or \code{\link{plot.basta}} median and 95\% predictive intervals for the estimated survival probability.}
	\item{mx }{If requested or called by functions \code{plot} or \code{\link{plot.basta}} median and 95\% predictive intervals for the estimated mortality rates.}
	\item{xv }{If requested, list with vectors of ages at death for each categorical covariate.}
  \item{bd }{If requested, the matrix with individual times of birth and death}
  \item{Y }{If requested, the capture-recapture matrix}
  \item{Za }{If requested, the matrix of categorical covariates. When no categorical covariates are included the model returns a one column matrix of 1's.}
  \item{Zc }{If requested, the matrix of continuous covariates. When no continuous covariates are included the model returns a one column matrix of 1's.}
  \item{ststart }{If requested, the first year of the study}
  \item{stend }{If requested, the last year of the study}
  \item{finished }{Vector of binary values indicating which simulations ran to the end.}
  \item{lifetable }{If requested and specified in the argument \code{lifetable}, a cohort life table calculated from the estimated ages at death.}
}
\references{
Colchero, F. and Clark, J.S. (\emph{in press}) Bayesian inference on age-specific survival from capture-recapture data for censored and truncated data. Journal of Animal Ecology. 

Colchero, F., Jones, O.R. and Rebke, M. BaSTA - a package to estimate mortality parameters from incomplete mark-recapture data with covariates. Unpublished Manuscript.

Gelman, A., Carlin, J.B., Stern, H.S. & Rubin, D.B. (2004) \emph{Bayesian data analysis}. 2nd edn. Chapman & Hall/CRC, Boca Raton, Florida, USA.

King, R. & Brooks, S.P. (2002) Bayesian model discrimination for multiple strata capture-recapture data. Biometrika, 89, 785-806. 

Spiegelhalter, D.J., Best, N.G., Carlin, B.P., Van Der Linde, A. (2002) Bayesian measures of model complexity and fit. Journal of the Royal Statistical Society: Series B 64, 583-639.
}
\author{
Fernando Colchero \email{colchero@demogr.mpg.de}, Owen R. Jones \email{jones@mpg.demogr.de} and Maren Rebke \email{rebke@mpg.demogr.de}
}

\seealso{
\code{\link{summary.basta}}, \code{\link{print.basta}}, \code{\link{plot.basta}} to visualized summary outputs for objects of class \dQuote{\code{basta}}.
\code{\link{CensusToCaptHist}} and \code{\link{makecovm}} for raw data formatting.
}
\examples{
## Load data:
data("basta.dat", package="BaSTA")

## Check data consistency:
new.dat  <- DataCheck(basta.dat, ststart=100, 
            stend=120, autofix=rep(1,7))

## Run short version of BaSTA on the data (+/- 1.5 mins.):
out      <- basta(basta.dat, ststart=100, stend=120, 
            model="GO", niter=2000, burnin=1001, 
            thinning=10, th.jumps=c(0.05,0.025), 
            ga.jumps=0.025, prog.plot=TRUE)

## Print results:
summary(out, digits=3)

## Plot traces for survival parameters:
plot(out)

## Plot traces for prop. hazards parameter:
plot(out, tracename="gamma")

## Plot survival and mortality curves:
plot(out, plot.trace=FALSE)
}

\keyword{ methods }
