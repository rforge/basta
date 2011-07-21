\name{bastaSummary}
\alias{bastaSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary function for BaSTA objects.
}
\description{
This function takes output objects created with functions \code{\link{bastaMCMC}} and \code{\link{bastaMCMCmulti}} to calculate summary statistics on estimated parameters and latent values (i.e. unknown times of birth and death), predictive intervals on survival probabilities and mortality rates, and MCMC convergence diagnostics.}
\usage{
bastaSummary(outBSM, lifetable = TRUE)
}

\arguments{
  \item{outBSM}{
An output object created with \code{\link{bastaMCMC}} or \code{\link{bastaMCMCmulti}}.
}
  \item{lifetable}{
Logical; if TRUE a life table (or array of life tables) is created from the average estimated ages at death (see details).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output includes four main lists: a) input; b) inference; c) qSuMo; and d) diagnost. The values for list 'input' are:
\item{model }{Character string specifying the mortality model used}
\item{Prop.Hazards }{Logical indicating if covariates were evaluated under a proportional hazards framework}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
  output$input           = list(modm = outBSM$i$modm, idm = outBSM$i$idm)
	output$inference$theta = list(mat=thmat, quantiles=thq)
	output$inference$gamma = list(mat=gamat, quantiles=gaq)
	output$inference$pi    = list(mat=pimat, quantiles=piq)
	output$inference$xq    = xq
	output$inference$bq    = bq
	output$qSuMo           = list(Sxsum = Sxq, mxsum = mxq, xza = xvec)
	output$diagnost        = list(model=outBSM$input$model, Prop.Hazards=outBSM$input$Prop.Hazards, ModSel = outBSM$diagnost$ModSel, convergence=conv, lifetable=LT)
}
\references{
Gelman, A., Carlin, J.B., Stern, H.S. & Rubin, D.B. (2004) Bayesian data analysis. 2nd edn. Boca Raton: Chapman & Hall/CRC.
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
function(outBSM, lifetable=TRUE){
	subrow     = function(x, id){
		if(length(dim(x))>2) nx = x[id,,] else nx = x[id,]
		return(nx)
	}
	nza        = ncol(outBSM$data$Za)
	nzc        = ncol(outBSM$data$Zc)
	thing      = seq(outBSM$input$burnin, outBSM$input$niter, outBSM$input$thinning)
	nthin      = length(thing)
	nsim       = ifelse(length(dim(outBSM$results$theta))>2, dim(outBSM$results$theta)[3], 1)
	idfull     = which(outBSM$diagnost$full.run)
	nth        = 5
	pname      = paste(rep(colnames(outBSM$input$modm),each=nza), "[",rep(colnames(outBSM$data$Za), nth),"]", sep="")
	zaname     = colnames(outBSM$data$Za)
	zcname     = colnames(outBSM$data$Zc)

	if(length(idfull)==nsim){
		# Mean, standard error and 95\% credible 
		# intervals for survival parameters:
		thm       = subrow(outBSM$results$theta, thing)
		if(nsim>1) thmat  = matrix(c(aperm(thm, c(1,3,2))),length(thing)*nsim, ncol(thm)) else thmat = thm
		colnames(thmat) = colnames(thm)
		thq       = cbind(apply(thmat,2,mean), 
		            apply(thmat,2, sd), 
		            t(apply(thmat, 2, 
		            quantile, c(0.025, 0.975))))
		colnames(thq) = c("Mean", "se", "2.5\%", "97.5\%")

		# Mean, standard error and 95\% credible 
		# intervals for proportional hazards parameters:
		if(!is.na(max(outBSM$results$gamma))){
			gam       = subrow(outBSM$results$gamma, thing)
			if(nsim>1) gamat  = matrix(c(aperm(gam, c(1,3,2))),length(thing)*nsim, ncol(outBSM$results$gamma)) else gamat = gam
			gaq       = cbind(apply(gamat,2,mean), 
			            apply(gamat,2, sd), 
			            t(apply(gamat, 2, 
			            quantile, c(0.025, 0.975))))
			colnames(gaq) = colnames(thq)
		} else {
			gaq    = NULL
			gamat  = NULL
		}
		# Mean, standard error and 95\% credible 
		# intervals for recapture parameters:
		
		if(ncol(outBSM$results$pi)==1){
			pim       = c(outBSM$results$pi)
			pimat     = pim
			piq       = matrix(c(mean(pim), 
			            sd(pim), quantile(pim, c(0.025, 0.975))), 1, ncol(thq))
			colnames(piq) = colnames(thq)
		} else {
			pim       = as.matrix(subrow(outBSM$results$pi, thing))
			pimat     = matrix(c(aperm(pim, c(1,3,2))),length(thing)*nsim, ncol(outBSM$results$pi))
			piq       = cbind(apply(pim,2,mean), 
			            apply(pim,2, sd), 
			            t(apply(pim, 2, 
			            quantile, c(0.025, 0.975))))
			colnames(piq) = colnames(thq)
		}
		
		# Median and upper and lower 95\% credible 
		#intervals for latent states (i.e. ages at death):
		xq        = apply(outBSM$results$xis, 2, 
		            quantile, c(0.5, 0.025, 0.975))
		bq        = apply(outBSM$results$bis, 2, 
		            quantile, c(0.5, 0.025, 0.975))
		
		# Convergence diagnostics (potential scale reduction):
		if(nsim>1){
			if(nzc>1){
				pmat  = array(NA, dim=c(nthin, ncol(thm)+ncol(gam), nsim))
				pmat[,1:ncol(thm),] = thm
				pmat[,ncol(thm)+1:ncol(gam),] = gam
				colnames(pmat) = c(paste("th.",colnames(thm),sep=""), paste("ga.",colnames(gam), sep=""))
			} else {
				pmat   = outBSM$results$theta[thing,,]
			}
			Means       = t(apply(pmat, c(2,3), mean))
			Vars        = t(apply(pmat, c(2,3), var))
			meanall     = apply(Means,2,mean)
			B           = nthin/(nsim-1)*apply(t((t(Means)-meanall)^2),2,sum)
			W           = 1/nsim*apply(Vars,2,sum)
			Varpl       = (nthin-1)/nthin * W + 1/nthin*B
			Rhat        = sqrt(Varpl/W)
			conv        = cbind(B,W,Varpl,Rhat)
			rownames(conv) = colnames(pmat)

			# assess convergence:
			idnconv     = which(conv[,'Rhat']< 0.95 | conv[,'Rhat']>1.2)
			if(length(idnconv)>0){
				warning("Convergence not reached for some survival parameters", call.=FALSE)
			} else {
				cat("Survival parameters converged appropriately.")
			} 
		} else {
			conv    = NULL
		}

		
		# Summary Survival and mortality functions:
		S.x         = function(th) Sx.fun(xv, matrix(th,1,nth), gaa, idm=outBSM$input$idm)
		m.x         = function(th) mx.fun(xv, matrix(th,1,nth), gaa, idm=outBSM$input$idm)

		# Median and 95\% predictive intervals for survival and mortality:
		Thmat      = matrix(0, length(thing)*nsim, nth * nza); colnames(Thmat) = pname
		if(nsim==1) Thmat[,colnames(thm)] = thm else Thmat[,colnames(thm)] = matrix(c(aperm(thm, c(1,3,2))),length(thing)*nsim,  ncol(thm), byrow=FALSE)
		if(!is.na(max(outBSM$results$gamma))){
			rzc       = apply(outBSM$data$Zc, 2, quantile, c(0.5, 0.025, 0.975))
			gave      = apply(gamat,2,mean)
			zcname    = paste("Cont.",c("Med.","Lower","Upper")[i],sep="")
		} else {
			rzc       = 0
			gave      = 0
			zcname    = c("")
		}
		Sxq       = list()
		mxq       = list()
		xvec      = list()
		for(i in 1:nza){
			idza    = which(outBSM$data$Za[,i]==1)
			xv      = seq(0,ceiling(max(xq[1,idza])*1.1),0.1)
			xvec[[zaname[i]]] = xv
			for(j in 1:length(rzc)){
				gaa    = sum(gave * rzc[j])
				Sxq[[zaname[i]]][[zcname[j]]] = apply(apply(Thmat[,paste(colnames(outBSM$input$modm), "[",zaname[i],"]", sep="")],1,S.x),1, quantile, c(0.5,0.025,0.975))
				
				mxq[[zaname[i]]][[zcname[j]]] = apply(apply(Thmat[,paste(colnames(outBSM$input$modm), "[",zaname[i],"]", sep="")],1,m.x),1, quantile, c(0.5,0.025,0.975))
			}
		}
		
		# Construct life table:
		if(lifetable){
			LT     = list()
			for(i in 1:nza){
				idza    = which(outBSM$data$Za[,i]==1 & bq[1,]>=outBSM$data$ststart)
				x       = xq[1,idza]
				LT[[colnames(outBSM$data$Za)[i]]] = CohortLT(x, ax=0.5, n=1)
			}
		} else {
			LT     = NULL
		}

	} else {
		thq       = NULL
		gaq       = NULL
		piq       = NULL
		xq        = NULL
		bq        = NULL
		Sxq       = NULL
		mxq       = NULL
		conv      = NULL
	}

	output                 = list()
	output$input           = list(modm = outBSM$i$modm, idm = outBSM$i$idm)
	output$inference$theta = list(mat=thmat, quantiles=thq)
	output$inference$gamma = list(mat=gamat, quantiles=gaq)
	output$inference$pi    = list(mat=pimat, quantiles=piq)
	output$inference$xq    = xq
	output$inference$bq    = bq
	output$qSuMo           = list(Sxsum = Sxq, mxsum = mxq, xza = xvec)
	output$diagnost        = list(model=outBSM$input$model, Prop.Hazards=outBSM$input$Prop.Hazards, ModSel = outBSM$diagnost$ModSel, convergence=conv, lifetable=LT)
	return(output)
	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
