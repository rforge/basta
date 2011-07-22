\name{basta}
\alias{basta}
\alias{basta.default}
\title{
Parametric Bayesian estimation of age-specific survival for truncated and censored capture-recapture/recovery records.
}
\description{
This function performs multiple Monte Carlo Markov Chain (MCMC) algorithms for Bayesian estimation of age-specific mortality and survival trends when a large proportion of (or all) records have unknown times of birth and/or death. Survival parameters and unknown (i.e. latent) birth and death times are estimated, with a choice of three mortality functions (i.e. Gompertz, Gompertz-Makeham and Siler), based on the model described by Colchero & Clark (2011). Categorical and continuous covariates can be included either within a proportional hazards framework or following Colchero and Clark's (2011) approach.}
\usage{
basta(Data, ststart, stend, ...)

basta.default(Data, ststart, stend, model = "SI", niter = 50000, burnin = 5001, thinning = 50, rptp = ststart, th.ini.pars = NULL, th.jumps = NULL, th.priors = NULL, Prop.Hazards = FALSE, ga.ini.pars = NULL, ga.jumps = NULL, ga.priors = NULL, nsim = 1, parallel = FALSE, ncpus = 2)
}
\arguments{
  \item{Data}{
A \code{data.frame} to be used as an input data file for BaSTA. The first is a column of individual unique ID's, the second and third columns are birth and death years respectively. Columns 4-(nt-1) represent the observation window of nt years. This is followed (optionally) by columns for categorical and continous covariates (see details). The data can be formated using functions \code{\link{CensusToCaptHist}} to build the capture-recapture matrix and \code{\link{makecovm}} for the covariates matrix.
}
  \item{ststart}{
The first year of the study.
}
  \item{stend}{
The last year of the study.
}
  \item{model}{
The underlying mortality model to be used. "GO"= Gompertz, "GM" = Gompertz-Makeham and "SI" = Siler (see .}
  \item{niter}{
The total number of MCMC steps (see details).
}
  \item{burnin}{
Number of iterations for the burn in (see details).
}
  \item{thinning}{
The thinning parameter.
}
  \item{rptp}{
A vector (of length npi) defining the recapture probability transition times. These are points (years) where the recapture probability is thought to change.  The default setting is for the recapture probability to be constant throughout the study, so the \code{rptp} is simply defined as a single element vector of the first year of the observation period (e.g. c(1985)). If one wanted to set a different recapture probability for years after (say) 1990, the RPTP could be defined as c(1985, 1990) etc..
}
  \item{th.ini.pars}{
A vector or matrix defining the initial values for each parameter on the survival model with the number of rows equal to the number of simulations (\code{nsim}, see below) to be ran (see details). The number of parameters for each simulation, nth, which determines the number of columns, will depend on the model selected (see details): a) Gompertz includes nth = two parameters of the form [alpha, beta]; b) Gompertz-Makeham has nth = three parameters with vector [c, alpha, beta]; and c) Siler with nth = five parameters as [alpha1, beta1, c, alpha2, beta2]. If the number of parameters specified does not match the number of parameters inherent to the model selected, the function returns an error. If no ini.pars.mat argument is specified (i.e. default is NULL), the model assigns a matrix of randomly generated initial parameters.
}
  \item{th.jumps}{
A vector or matrix defining the size of jumps for each survival model parameter (standard error in the Metropolis step, see details). The number of parameters, nth, will depend on the model selected.
}
  \item{th.priors}{
A vector or matrix defining the priors for each survival parameter. The vector takes the same format as described for \code{jumps}.
}
  \item{Prop.Hazards}{
A logical argument that indicates if covariates should be evaluated under a proportional hazards framework. Default is FALSE. 
}
  \item{ga.ini.pars}{
A vector of initial parameters for the proportional hazards section of the model with the number of rows equal to the number of simulations (\code{nsim}, see below) to be ran (see details). These can be specified when Prop.Hazards is TRUE or when continuous covariates are evaluated. As with \code{th.ini.pars}, the default is set to NULL.
}
  \item{ga.jumps}{
 A vector of jump standard errors for the proportional hazards parameters. The number of jumps will be equal to the number of covariates when the argument \code{Prop.Hazards} is set to TRUE, or to the number of continuous covariates to be evaluated. In case any of these two conditions are met and the default is used (i.e. NULL), the model automatically assigns builtin jump values.
}
  \item{ga.priors}{
 A vector of mean priors for the proportional hazards parameters. The specification follows the same rules as \code{cjumps}.
}
  \item{nsim}{
A numerical value for the number of simulations to be ran.
}
  \item{parallel}{
Logical; if TRUE package \pkg{snowfall} is called and multiple simulations are run in parallel. If \pkg{snowfall} is not installed, the model is ran in series.
}
  \item{ncpus}{
A numerical value that indicates the number of cpus to be used if \code{parallel} is TRUE and package \pkg{snowfall} is installed. The default is 2 cpus. 
}
  \item{lifetable}{Logical, if \code{TRUE} a cohort life table is calculated using function \code{\link{CohortLT}}.
  }
}
\details{
 Under construction...
}
\value{
\code{basta} returns an object of class '\code{basta}'. The function \code{\link{summary}} (i.e. \code{\link{summary.glm}}) can be used to obtain or print a summary of the results.
  \item{coefficients }{Matrix with estimated coefficients (i.e. mean values per parameter on the thinned sequences after burnin), which includes standard errors, upper and lower 95\% credible intervals, update rates per parameter (commonly the same for all survival and proportional hazards parameters), serial autocorrelation on the thinned sequences and potential scale reduction factor for convergence (see \code{Convergence} value below). }
	\item{ModSel }{Basic DIC calculations for model selection (Spiegelhalter \emph{et al.} 2002) These values need to be taken only as a reference (see comments to Spiegelhalter \emph{et al.} 2002). In the future the package will include routines for reversible jump Monte Carlo Markov Chain (RJMCMC, see King & Brooks 2002). If none or some of the simulations did not run through, then the returned value is 'NULL'}
	\item{Convergence }{If called by \code{\link{summary}}, matrix with convergence coefficients based on potential scale reduction as described by Gelman \emph{et al.} (2004). If only one simulation was ran, then the returned value is 'NULL'}
  \item{settings }{If called by \code{\link{summary}}, vector indicating the number of iterations for each MCMC, the burn in sequence, the thinning interval and the number of simulations ran, as specified by the user.}
	\item{ModelSpecs }{Model specifications inidicating the model used (i.e. Gompertz = "GO", Gompertz-Makeham = "GM", or Siler = "SI") as specified by the user.}
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
King, R. & Brooks, S.P. (2002) Bayesian model discrimination for multiple strata capture-recapture data. Biometrika, 89, 785–806. 
Spiegelhalter, D.J., Best, N.G., Carlin, B.P., Van Der Linde, A. (2002) Bayesian measures of model complexity and fit. Journal of the Royal Statistical Society: Series B 64, 583-639.
}
\author{
Fernando Colchero \email{colchero@demogr.mpg.de}
}

\seealso{
\code{\link{summary.basta}}, \code{\link{print.basta}}, \code{\link{plot.basta}} to visualized summary outputs for objects of class '\code{basta}'.
\code{\link{CensusToCaptureHistory}} and \code{\link{makecovm}} for raw data formatting.
}
\examples{
  print("under construction...")
}

\keyword{ methods }
