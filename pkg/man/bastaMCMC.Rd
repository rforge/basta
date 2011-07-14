\name{bastaMCMC}
\alias{bastaMCMC}

\title{A function to perform parametric Bayesian estimation of age-specific mortality and survival trends from capture-recapture/recovery data with a large proportion of truncated and censored records.}
\description{
This function performs Bayesian estimation of age-specific mortality and survival trends when a large proportion (or all) of records have unknown times of birth and/or death. Survival parameters and unknown (i.e. latent) birth and death times are estimated, with a choice of three mortality functions (i.e. Gompertz, Gompertz-Makeham and Siler), based on the model described by Colchero & Clark (2011). Categorical and continuous covariates can be included either within a proportional hazards framework or following Colchero and Clark's (2011) approach.
}
\usage{
bastaMCMC(Data, ststart, stend, model = "SI", niter = 50000, burnin = 5001, thinning = 50, rptp = ststart, ini.pars = NULL, jumps = NULL, priors = NULL, Prop.Hazards = FALSE, cini.pars = NULL, cjumps = NULL, cpriors = NULL, datacheck = TRUE)
}
\arguments{
  \item{Data}{
A \code{data.frame} to be used as an input data file for BaSTA. The first is a column of individual unique ID's, the second and third columns are birth and death years respectively. Columns 4-(nt-1) represent the observation window of nt years. This is followed (optionally) by columns for categorical and continous covariates (see details). The data can be formated using functions \code{\link{CensusToCaptHist}} to build the capture-recapture matrix and \code{\link{makecovm}} for the covariates matrix (or \code{\link{designmat}} in case an object of class \code{\link{"formula"}} is used).
}
  \item{ststart}{
The first year of the study.
}
  \item{stend}{
The last year of the study.
}
  \item{model}{
The underlying mortality model to be used. "GO"= Gompertz, "GM" = Gompertz-Makeham and "SI" = Siler (see details).}
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
  \item{ini.pars}{
A vector defining the initial values for each parameter. The number of parameters, nth, will depend on the model selected (see details): a) Gompertz includes nth = two parameters of the form [alpha, beta]; b) Gompertz-Makeham has nth = three parameters with vector [c, alpha, beta]; and c) Siler with nth = five parameters as [alpha1, beta1, c, alpha2, beta2]. If the number of parameters specified does not match the number of parameters inherent to the model selected, the function returns an error. If no ini.pars argument is specified (i.e. default is NULL), the model assigns a vector of builtin initial parameters.}
  \item{jumps}{
A vector defining the size of jumps for each survival model parameter (standard error in the Metropolis step, see details). The vector takes the same format as described for \code{ini.pars}.
}
  \item{priors}{
A vector defining the priors for each parameter. The vector takes the same format as described for \code{ini.pars}.}
}
  \item{Prop.Hazards}{
A logical argument that indicates if covariates should be evaluated under a proportional hazards framework. Default is FALSE. 
}
  \item{cini.pars}{
A vector of initial parameters for the proportional hazards section of the model. These can be specified when Prop.Hazards is TRUE or when continuous covariates are evaluated. As with \code{ini.pars}, the default is set to NULL.
}
  \item{cjumps}{
 A vector of jumps for cini.pars. The specification follows the same rules as cini.pars.
}
  \item{cpriors}{
 A vector of priors for cini.pars. The specification follows the same rules as cini.pars.
}
  \item{datacheck}{
A logical argument specifying whether the input data should be checked for errors or not.}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
The output includes four main lists: a) data; b) input; c) results; and d) diagnost. The values for list 'data' are:
\item{bd }{Matrix with individual times of birth and death}
\item{Y }{Capture-recapture matrix}
\item{Za }{Matrix of categorical covariates. When no categorical covariates are included the model returns a one column matrix of 1's.}
\item{Zc }{Matrix of continuous covariates. When no continuous covariates are included the model returns a one column matrix of 1's.}
\item{ststart }{Starting year of the study}
\item{stend }{Final study year}

The values for list 'input' are:
\item{niter }{Total number of MCMC iterations}
\item{burnin }{Burn in sequence}
\item{thinning }{Number of steps to be descarded to reduce serial autocorrelation}
\item{model }{Character string specifying the mortality model used}
\item{ini.pars }{Initial parameters}
\item{jumps }{Jump standard deviations for survival parameters}
\item{priors }{Priors for survival parameters}
\item{Prop.Hazards }{Logical for proportional hazards model}
\item{cini.pars }{Initial parameters for proportional hazards section}
\item{cjumps }{Jump standard errors for proportional hazards parameters}
\item{cpriors }{Priors for proportional hazards parameters}

The values for list 'results' are:
\item{theta }{An \code{niter} by nth * nza matrix of estimated survival parameters}
\item{gamma }{An \code{niter} by nzc matrix of estimated proportional hazards parameters}
\item{pi }{An \code{niter} by npi matrix of estimated recapture probabilities.}
\item{bis }{An \code{niter} by n matrix of estimated times of birth}
\item{xis }{An \code{niter} by n matrix of estimated ages at death}
\item{post }{An \code{niter} by 3 matrix of posteriors. The first column includes conditional posterior values for survival and proportional hazard parameters, the second includes conditional posterior values for the estimated latent states (i.e. unknown times of birth and death) and last column if the full model posterior.}

The values for list 'diagnost' are:
\item{full.run }{An indicator variable that assigns 1 if the model ran though and 0 if the model stoped if it did not run until the \code{niter} step.}
\item{last.step }{The last step ran in the model; if the model runs through the value assigned is equal to \code{niter}.}
\item{ModSel }{Basic DIC calculations for model selection (Spiegelhalter \emph{et al.} 2002) These values need to be taken only as a reference (see comments to Spiegelhalter \emph{et al.} 2002). In the future the package will include routines for reversible jump Monte Carlo Markov Chain (RJMCMC, King & Brooks 2002).}
}
\references{
Colchero, F. and Clark, J.S. (2011) Bayesian inference on age-specific survival from capture-recapture data for censored and truncated data. Journal of Animal Ecology. 
Colchero, F., Jones, O.R. and Rebke, M. BaSTA - a package to estimate mortality parameters from incomplete mark-recapture data with covariates. Unpublished Manuscript.
King, R. & Brooks, S.P. (2002) Bayesian model discrimination for multiple strata capture-recapture data. Biometrika, 89, 785–806. 
Spiegelhalter, D.J., Best, N.G., Carlin, B.P., Van Der Linde, A. (2002) Bayesian measures of model complexity and fit. Journal of the Royal Statistical Society: Series B 64 (4), 583-639.
}
\author{
Fernando Colchero \email{colchero@mpg.demogr.de}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bastaMCMCmulti}}, \code{\link{plot.traces}}, \code{\link{plot.SM}}, \code{\link{DataCheck}}
}
\examples{
Here is an example...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
