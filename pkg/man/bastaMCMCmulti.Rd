\name{bastaMCMCmulti}
\alias{bastaMCMCmulti}

\title{
Multiple parametric Bayesian estimations of age-specific survival for truncated and censored capture-recapture records.
}
\description{
This function performs multiple Monte Carlo Markov Chain (MCMC) algorithms for Bayesian estimation of age-specific mortality and survival trends when a large proportion of (or all) records have unknown times of birth and/or death. Survival parameters and unknown (i.e. latent) birth and death times are estimated, with a choice of three mortality functions (i.e. Gompertz, Gompertz-Makeham and Siler), based on the model described by Colchero & Clark (2011). Categorical and continuous covariates can be included either within a proportional hazards framework or following Colchero and Clark's (2011) approach. Multiple simulations can then be used to evaluate convergence using \code{\link{bastaSummary}}.}
\usage{
bastaMCMCmulti(Data, ststart, stend, model = "SI", niter = 50000, burnin = 5001, thinning = 50, rptp = ststart, ini.pars.mat = NULL, jumps = NULL, priors = NULL, Prop.Hazards = FALSE, cini.pars.mat = NULL, cjumps = NULL, cpriors = NULL, nsim = 5, parallel = FALSE, ncpus = 2)
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
  \item{ini.pars.mat}{
A matrix defining the initial values for each parameter on the survival model with the number of rows equal to the number of simulations (\code{nsim}, see below) to be ran (see details). The number of parameters for each simulation, nth, which determines the number of columns, will depend on the model selected (see details): a) Gompertz includes nth = two parameters of the form [alpha, beta]; b) Gompertz-Makeham has nth = three parameters with vector [c, alpha, beta]; and c) Siler with nth = five parameters as [alpha1, beta1, c, alpha2, beta2]. If the number of parameters specified does not match the number of parameters inherent to the model selected, the function returns an error. If no ini.pars.mat argument is specified (i.e. default is NULL), the model assigns a matrix of randomly generated initial parameters.}
}
  \item{jumps}{
A vector defining the size of jumps for each survival model parameter (standard error in the Metropolis step, see details). The number of parameters, nth, will depend on the model selected.
}
  \item{priors}{
A vector defining the priors for each survival parameter. The vector takes the same format as described for \code{jumps}.}
}
  \item{Prop.Hazards}{
A logical argument that indicates if covariates should be evaluated under a proportional hazards framework. Default is FALSE. 
}
  \item{cini.pars}{
A matrix of initial parameters for the proportional hazards section of the model with the number of rows equal to the number of simulations (\code{nsim}, see below) to be ran (see details). These can be specified when Prop.Hazards is TRUE or when continuous covariates are evaluated. As with \code{ini.pars.mat}, the default is set to NULL.
}
  \item{cjumps}{
 A vector of jump standard errors for the proportional hazards parameters. The number of jumps will be equal to the number of covariates when the argument \code{Prop.Hazards} is set to TRUE, or to the number of continuous covariates to be evaluated. In case any of these two conditions are met and the default is used (i.e. NULL), the model automatically assigns builtin jump values.
}
  \item{cpriors}{
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
}
\details{
The model 
}
\value{
\code{basta} returns an object of class '\code{basta}'. The function \code{\link{summary}} (i.e. \code{\link{summary.glm}}) can be used to obtain or print a summary of the results.
The output includes four main lists: a) data; b) input; c) results; and d) diagnost. These outputs are compatible with those from \code{\link{bastaMCMC}}. The values for list 'data' are:
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
\item{ini.pars.mat }{Matrix of initial parameters where each row corresponds to }
\item{jumps }{Jump standard deviations for survival parameters}
\item{priors }{Priors for survival parameters}
\item{Prop.Hazards }{Logical for proportional hazards model}
\item{cini.pars.mat }{Initial parameters for proportional hazards section}
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
\item{ModSel }{Basic DIC calculations for model selection (Spiegelhalter \emph{et al.} 2002) These values need to be taken only as a reference (see comments to Spiegelhalter \emph{et al.} 2002). In the future the package will include routines for reversible jump Monte Carlo Markov Chain (RJMCMC, see King & Brooks 2002).}
}
\references{
Colchero, F. and Clark, J.S. (2011) Bayesian inference on age-specific survival from capture-recapture data for censored and truncated data. Journal of Animal Ecology. 
Colchero, F., Jones, O.R. and Rebke, M. BaSTA - a package to estimate mortality parameters from incomplete mark-recapture data with covariates. Unpublished Manuscript.
King, R. & Brooks, S.P. (2002) Bayesian model discrimination for multiple strata capture-recapture data. Biometrika, 89, 785–806. 
Spiegelhalter, D.J., Best, N.G., Carlin, B.P., Van Der Linde, A. (2002) Bayesian measures of model complexity and fit. Journal of the Royal Statistical Society: Series B 64, 583-639.
}
\author{
Fernando Colchero \email{colchero@demogr.mpg.de}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Data, ststart, stend, model="SI", niter=50000, burnin=5001, thinning=50, rptp = ststart, ini.pars.mat=NULL, jumps=NULL, priors=NULL, Prop.Hazards = FALSE, cini.pars.mat=NULL, cjumps=NULL, cpriors=NULL, nsim=5, parallel=FALSE, ncpus=2){

	# Packages:
	require(msm)

	# Data error checking:
	tempcheck   = DataCheck(Data, ststart, stend, silent=TRUE)
	if(tempcheck[[1]] == FALSE) stop("You have an error in Dataframe 'Data',\nplease use function 'DataCheck'\n", call.=FALSE)

    #Check that niter, burnin, and thinning are compatible.
    if(burnin>niter) stop("\nObject 'burnin' larger than 'niter'.")
    if(thinning>niter) stop("\nObject 'thinning' larger than 'niter'.")

	# Check that nsim is larger than 1:
	if(nsim<=1) stop("\nObject 'nsim' needs to be larger than 1\nFor a single run use function 'bastaMCMC'.")

	# Model Matrix and boundary values for parameters:
	nth         = 5
	modm        = matrix(c(0,0,1,0,0,1,0,rep(1,8)), 3, nth, dimnames=list(c("GO", "GM", "SI"), c("alpha1", "beta1", "c", "alpha2", "beta2")))
	idm         = which(rownames(modm)==model)
	nthm        = sum(modm[idm,])
	th.low      = matrix(-Inf, nrow(modm),nth, dimnames=dimnames(modm))
	th.low["SI",c("beta1","beta2")] = 0
	
	# Construct initial parameters matrix:
	if(is.null(ini.pars.mat)){
		ini.pars.mat    = matrix(0, nsim, nth)
		for(i in 1:nsim){
			ini.pars.mat[i,] = rtnorm(nth, c(-1, 0.001, 0, -1, 0.001), 0.25, lower=th.low[idm,])
			if(idm>1){
				clow     = c.low(ini.pars.mat[i,], idm=idm)
				if(ini.pars.mat[i,3]<clow) ini.pars.mat[i,3] = rtnorm(1, 0, 0.5, lower=clow)
			}
		}
		ini.pars.mat  = ini.pars.mat[,modm[idm,]==1]
	} else {
		if(ncol(ini.pars.mat)!=nthm){
			stop(paste("\nWrong dimemnsions in 'ini.pars.mat'.\nFor model", model, "initial parameters matrix should have", nthm, "columns.\n"), call.=FALSE)
		}
	}

	# Evaluate proportional hazards initial parameters:
	nt      = length(ststart:stend)
	if(ncol(Data)>nt+3){
		Z         = as.matrix(Data[,(nt+4):ncol(Data)])
		Ith       = Ith.fun(Z)
		if(Prop.Hazards | !is.null(Ith$cont)){
			if(Prop.Hazards) nzc = ncol(Z) else nzc = length(Ith$cont) + 1
			Cont    = TRUE
			if(is.null(cini.pars.mat)){
				cini.pars.mat    = matrix(rnorm(nzc*nsim, 0, 0.5), nsim, nzc)
			} else {
				if(ncol(cini.pars.mat)!=nzc) stop(paste("\nWrong dimemnsions of initial parameter matrix (i.e. 'cini.pars.mat') for proportional hazards. Model should have", nzc, "columns.\n"), call.=FALSE)
			} 
		} else {
			cini.pars.mat  = matrix(0,nsim,1)
			nzc            = 1
			Cont           = FALSE
		}
	} else {
		cini.pars.mat  = matrix(0,nsim,1)
		nzc            = 1
		Cont           = FALSE
	}

	# Parallel function:
	paralBSM  = function(sim){
		if(parallel) for(ii in 1:(sim*2)){}
#		env     = new.env()
		Ypos    = (75*sim) - 75
		outbsm  = bastaMCMC(Data=Data, ststart=ststart, stend=stend, model=model, niter=niter, burnin=burnin, thinning=thinning, rptp=rptp, ini.pars=ini.pars.mat[sim,], jumps=jumps, priors=priors, Prop.Hazards=Prop.Hazards, cini.pars=cini.pars.mat[sim,], cjumps=cjumps, cpriors=cpriors,  datacheck=FALSE)
		return(outbsm)
	}

	# Run multiple BayesSurv simulations:
	Start      = Sys.time()
	cat("\nMultiple simulations started.\n")
	if(parallel){
		availpkg     = available.packages()
		if(!is.element("snowfall", availpkg)){
			warning("\nPackage 'snowfall' is not installed.\nSimulations will not be ran in parallel (computing time will be longer...)\n")
			outBSM   = lapply(1:nsim, paralBSM)
			names(outBSM) = paste("Sim.", 1:nsim)
		} else {
			require(snowfall)
			sfInit(parallel=TRUE, cpus=ncpus);
			sfExport("Data", "ststart", "stend", "model", "niter", "burnin", "thinning", "rptp", "ini.pars.mat", "jumps", "priors", "Prop.Hazards", "cini.pars.mat", "cjumps", "cpriors", "bastaMCMC", "DataCheck", "fx.fun", "mx.fun", "Sx.fun", "ObsMatFun", "c.low","parallel","Ith.fun")
			sfLibrary(msm)
			outBSM = sfClusterApplyLB(1:nsim, paralBSM)
			sfStop()
			names(outBSM) = paste("Sim.", 1:nsim)
		}
	} else {
		outBSM   = lapply(1:nsim, paralBSM)
		names(outBSM) = paste("Sim.", 1:nsim)
	}

	# Report running time:
	End        = Sys.time()
	cat(paste("\nMultiple MCMC computing time: ", round(as.numeric(julian(End)-julian(Start))*24*60, 2), " minutes\n", sep=""))

	# Report if simo simulations failed:
	full.runs  = rep(0,nsim)
	last.steps = full.runs
	for(i in 1:nsim){
		full.runs[i]  = ifelse(outBSM[[i]]$diagnost$full.run, 1, 0)
		last.steps[i] = outBSM[[i]]$diagnost$last.step
	} 
	id.failed  = which(full.runs==0)
	id.ran     = which(full.runs==1)
	all.ran    = FALSE
	if(length(id.failed)>0 & length(id.failed)<nsim){
		cat("\nOne or more simulations failed\nConvergence diagnostics could not be calculates\n")
	} else if(length(id.failed)==nsim){
		cat("\nAll simulations failed\nConvergence diagnostics will not be calculates\n")
	} else {
		all.ran    = TRUE
		cat("\nMultiple simulations finished.\n")
	}
	

	# Collect results:
	simname    = paste("Sim.", (1:nsim), sep="")
	tnth       = sum(modm[idm,])*ncol(outBSM[[1]]$data$Za)
	tnpi       = length(rptp)
	tnni       = nrow(Data)
	tnpo       = 3
	thing      = seq(burnin, niter, thinning)
	nthin      = length(thing)
	
	thmat      = array(NA, dim=c(niter, tnth, nsim), dimnames=list(NULL, colnames(outBSM[[1]]$results$theta), simname))
	gamat      = array(NA, dim=c(niter, nzc, nsim), dimnames=list(NULL, colnames(outBSM[[1]]$results$gamma), simname))
	pimat      = array(NA, dim=c(niter, tnpi, nsim), dimnames=list(NULL, colnames(outBSM[[1]]$results$pi), simname)) 
	bimat      = array(NA, dim=c(nthin, tnni, nsim), dimnames=list(NULL, NULL, simname))
	ximat      = array(NA, dim=c(nthin, tnni, nsim), dimnames=list(NULL, NULL, simname))
	pomat      = array(NA, dim=c(niter, tnpo, nsim), dimnames=list(NULL, colnames(outBSM[[1]]$results$post), simname))
	DImat      = matrix(NA, nsim, 5, dimnames = list(simname, names(outBSM[[1]]$diagnost$ModSel))) 
	
	for(i in 1:nsim){
		thmat[,,i]  = outBSM[[i]]$results$theta
		gamat[,,i]  = outBSM[[i]]$results$gamma
		pimat[,,i]  = outBSM[[i]]$results$pi
		bimat[,,i]  = outBSM[[i]]$results$bis
		ximat[,,i]  = outBSM[[i]]$results$xis
		pomat[,,i]  = outBSM[[i]]$results$post
		if(full.runs[i]==0){
			DImat[i,]   = rep(NA, 5)
		} else {
			DImat[i,]   = outBSM[[i]]$diagnost$ModSel
		}
	} 

	full.run    = full.runs==1

	#Return a list object
	output          = list()
	output$data     = list(bd = outBSM[[1]]$data$bd,Y = outBSM[[1]]$data$Y,Za = outBSM[[1]]$data$Za,Zc = outBSM[[1]]$data$Zc, ststart=ststart, stend=stend)
	output$input    = list(niter=niter, burnin = burnin, thinning = thinning, model=model, modm=modm, idm=idm, ini.pars=ini.pars.mat, jumps=jumps, priors=priors, Prop.Hazards=Prop.Hazards, cini.pars=cini.pars.mat, cjumps=cjumps, cpriors=cpriors)
	output$results  = list(theta=thmat, gamma=gamat, pi = pimat, bis = bimat, xis = ximat, post=pomat)
	output$diagnost = list(full.run=full.run, last.step=last.steps, ModSel = DImat)
	return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
