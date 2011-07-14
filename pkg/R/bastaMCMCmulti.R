bastaMCMCmulti <-
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
#		Ypos    = (75*sim) - 75
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
	simname    = paste("Sim.", (1:nsim), sep="")
	full.runs  = rep(0,nsim)
	last.steps = full.runs
	Thgini     = array(NA, dim=c(ncol(outBSM[[1]]$data$Za), nthm, nsim))
	dimnames(Thgini) = list(colnames(outBSM[[1]]$data$Za), colnames(outBSM[[1]]$input$ini.pars, simname))
	Thjini     = Thgini
	Thpini     = Thgini
	Gagini     = matrix(NA, nsim, ncol(outBSM[[1]]$data$Zc), dimnames=list(simname, names(outBSM[[1]]$input$cini.pars)))
	for(i in 1:nsim){
		full.runs[i]  = ifelse(outBSM[[i]]$diagnost$full.run, 1, 0)
		last.steps[i] = outBSM[[i]]$diagnost$last.step
		Thgini[,,i]   = outBSM[[i]]$input$ini.pars
		Thjini[,,i]   = outBSM[[i]]$input$jumps
		Thpini[,,i]   = outBSM[[i]]$input$priors
		Gagini[i,]    = outBSM[[i]]$input$cini.pars
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
	output$input    = list(niter=niter, burnin = burnin, thinning = thinning, model=model, modm=modm, idm=idm, ini.pars=Thgini, jumps=Thjini, priors=Thpini, Prop.Hazards=Prop.Hazards, cini.pars=Gagini, cjumps=outBSM[[1]]$input$cjumps, cpriors=outBSM[[1]]$input$cpriors)
	output$results  = list(theta=thmat, gamma=gamat, pi = pimat, bis = bimat, xis = ximat, post=pomat)
	output$diagnost = list(full.run=full.run, last.step=last.steps, ModSel = DImat)
	return(output)
}

