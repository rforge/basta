bastaMCMC <-
function(Data, ststart, stend, model="SI", niter=50000, burnin=5001, thinning=50, rptp = ststart, ini.pars=NULL, jumps=NULL, priors=NULL, Prop.Hazards = FALSE, cini.pars=NULL, cjumps=NULL, cpriors=NULL, datacheck=TRUE){
	require(msm)
	# 1. Data check:
	if(datacheck){
		tempcheck   = DataCheck(Data, ststart, stend, silent=TRUE)
		if(tempcheck[[1]] == FALSE) stop("You have an error in Dataframe 'Data',\nplease use function 'DataCheck'", call.=FALSE) 
	}
	
    # 2. Check that niter, burnin, and thinning are compatible.
    if(burnin>niter) stop("\nObject 'burnin' larger than 'niter'.", call.=FALSE)
    if(thinning>niter) stop("\nObject 'thinning' larger than 'niter'.", call.=FALSE)
	
	# Data formating:
	Ti          = ststart
	Tf          = stend
	st          = Ti:Tf
	nt          = length(st)
	idnames     = Data[,1]
	n           = nrow(Data)
	bd          = as.matrix(Data[,2:3])
	Y           = as.matrix(Data[,1:nt+3]); colnames(Y) = st

	# Extract covariates:
	# a) Find if there are covariates:
	if(ncol(Data)>nt+3){
		Z         = as.matrix(Data[,(nt+4):ncol(Data)])
		Ith       = Ith.fun(Z)
		Covars    = TRUE
		
		# Find if the model is Prop. Hazards:
		if(Prop.Hazards){
			Zc     = Z
			Za     = matrix(1, n, 1); colnames(Za) = "NZa"
			Cat    = FALSE
			Cont   = TRUE
		} else {
		
			# Find if there are continuous covariates:
			if(!is.null(Ith$cont)){
				Zc     = cbind(1,Z[,Ith$cont])
				colnames(Zc) = c("Int", colnames(Z)[Ith$cont])
				Cont   = TRUE
			} else {
				Zc     = matrix(0,n,1); colnames(Zc) = "NZc"
				Cont   = FALSE
			}
		
			# Find if there are categorical covariates:
			if(!is.null(Ith$cat)){
				Za     = Z[,Ith$cat]
				if(!is.null(Ith$int)){
					Za   = cbind(1,Za)
					colnames(Za) = c("Int", colnames(Za)[-1])
				}
				Cat    = TRUE
			} else {
				Za     = matrix(1, n, 1); colnames(Za) = "NZa"
				Cat    = FALSE
			}
		}
	} else {
		Z         = NULL
		Za        = matrix(1, n, 1); colnames(Za) = "NZa"
		Zc        = Za; colnames(Zc) = "NZc"
		Prop.Hazards = FALSE
		Covars    = FALSE
		Cat       = FALSE
		Cont      = FALSE
	}
	nza         = ncol(Za)
	nzc         = ncol(Zc)

	# Model Matrix and lower limits for parameters:
	nth         = 5
	modm        = matrix(c(0,0,1,0,0,1,0,rep(1,8)), 3, nth, dimnames=list(c("GO", "GM", "SI"), c("alpha1", "beta1","c","alpha2","beta2")))
	pname       = paste(rep(colnames(modm),each=nza), "[",rep(colnames(Za), nth),"]", sep="")
	idm         = which(rownames(modm)==model)
	idth        = which(modm[rep(idm, nza),]==1)
	nthm        = sum(modm[idm,])
	th.low      = matrix(-Inf, nrow(modm),nth, dimnames=dimnames(modm))
	th.low["SI",c("beta1","beta2")] = 0
	low         = matrix(th.low[idm,],nza, nth, byrow=TRUE)
	dimnames(low) = list(colnames(Za), colnames(modm))

	# Model variables:
	diffrec     = rptp 
	ng          = niter
	bng         = burnin
	thint       = thinning

	# Verify jumps, initial parameters and priors:
	if(is.null(jumps)){
		thj       = t(t(modm[rep(idm, nza),])*c(0.005, 0.005, 0.02, 0.0075, 0.001))
		if(nza==1) thj = t(thj)
	} else {
		ljumps    = length(jumps)
		if(ljumps/nthm != round(ljumps/nthm)){
			stop(paste("\nLength of jumps for the mortality model parameters is not a multiple of ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(ljumps > nth*nza) {
			stop(paste("\nLength of jumps for the mortality model parameters is larger than ", nthm * nza, " (i.e. number of parameters for model ", model," times number of covariates)", sep=""), call.=FALSE)		}
		thj       = modm[rep(idm, nza), ]
		thj[,modm[idm,]==1] = matrix(t(jumps), nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thj = t(thj)
	}

	if(is.null(ini.pars)){
		thg       = t(t(modm[rep(idm, nza),])*c(-1, 0.001, 0, -1, 0.001))
		if(nza==1) thg = t(thg)
	} else {
		lini.pars = length(ini.pars)
		if(lini.pars/nthm != round(lini.pars/nthm)){
			stop(paste("\nLength of starting parameters (argument 'ini.pars') for the mortality model is not a multiple of ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(lini.pars > nth*nza) {
			stop(paste("\nLength of starting parameters (argument 'ini.pars') for the mortality model is larger than ", nthm * nza, " (i.e. number of parameters for model ", model," times number of covariates)", sep=""), call.=FALSE)		
		}
		thg       = modm[rep(idm, nza), ]
		thg[,modm[idm,]==1] = matrix(t(ini.pars),nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thg = t(thg)
	}	
	
	if(is.null(priors)){
		thp       = t(t(modm[rep(idm, nza),])*c(-5,0.1,-1,0.001,0.005))
		if(nza==1) thp = t(thp)
	} else {
		lpriors   = length(priors)
		if(lpriors/nthm != round(lpriors/nthm)){
			stop(paste("\nLength of priors for the mortality model parameters is not a multiple of ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(lpriors > nth*nza) {
			stop(paste("\nLength of priors for the mortality model parameters is larger than ", nthm * nza, " (i.e. number of parameters for model ", model," times number of covariates)", sep=""), call.=FALSE)		}
		thp       = modm[rep(idm, nza), ]
		thp[,modm[idm,]==1] = matrix(t(priors),nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thp = t(thp)
	}
	
	
	# Define proportions hazards section:
	if(Cont){
		if(is.null(cini.pars)){
			gag     = rep(0, nzc)
			names(gag) = colnames(Zc)
		} else {
			lcini.pars = length(cini.pars)
			if(lcini.pars != nzc){
				stop(paste("\nLength of prop. hazards parameters is not equal to number of covariates (n = ", nzc,").", sep=""), call.=FALSE)
			} else {
				gag    = cini.pars
				names(gag) = colnames(Zc)
			}
		}
		
		if(is.null(cjumps)){
			gaj     = rep(0.0001, nzc)
			names(gaj) = colnames(Zc)
		} else {
			lcjumps = length(cjumps)
			if(lcjumps != nzc){
				stop(paste("\nLength of jumps for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
			} else {
				gaj    = cjumps
				names(gaj) = colnames(Zc)
			} 
		}
		
		if(is.null(cpriors)){
			gap     = rep(1, nzc)
			names(gap) = colnames(Zc)
		} else {
			lcpriors = length(cpriors)
			if(lcpriors != nzc){
				stop(paste("\nLength of priors for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
			} else {
				gap    = cjumps
				names(gap) = colnames(Zc)
			} 
		}
		
	} else {
		gag    = 0
		gaj    = 0
		gap    = 0
	}

		
	# Model variables:
	# Extract times of birth and death:
	bi          = bd[,1]
	di          = bd[,2]

	# Define study duration:
	Dx          = (st[2]-st[1])
	Tm          = matrix(st, n, nt, byrow=TRUE)

	# Calculate first and last time observed:
	ytemp       = t(t(Y) * st)
	li          = c(apply(ytemp,1,max))
	ytemp[ytemp==0] = 10000
	fi          = c(apply(ytemp,1,min))
	fi[fi==10000] = 0
	rm("ytemp")

	# Calculate number of times detected:
	oi          = Y %*% rep(1, nt)

	# Define priors:
	# Priors for survival parameters:
	Zthp        = Za %*% thp
	thv         = 0.5
		
	# Priors for prop. hazards part:
	Zgap        = Zc %*% gap
	gav         = 1

	# Prior for age distribution:
	dxx         = 0.001
	xx          = seq(0,100,dxx)
	zza         = cbind(1,matrix(0, length(xx), nza-1))
	zzc         = sum(apply(Zc, 2, mean) * gap) 
	Ex          = sum(xx*fx.fun(xx,zza %*% thp, zzc, idm=idm)*dxx)
	v.x         = function(x) Sx.fun(x,Zthp, Zgap, idm=idm)/Ex

	# Prior parameters for detection probability:
	idpi        = findInterval(st, diffrec); names(idpi) = st
	npi         = length(unique(idpi))
	rho1        = 0.1
	rho2        = 0.1


	# Starting values:
	# Survival parameters
	Zthg        = Za %*% thg
		
	# Prop. hazards parameter:
	Zgag        = Zc %*% gag

	# Recapture probability:
	pig         = rep(0.5, npi)
	Pig         = pig[idpi]

	# Times of birth and death:
	bi0         = which(bi==0)
	bg          = bi
	bg[bi==0 & fi>0] = fi[bi==0 & fi>0] - 1
	bg[bi==0 & fi==0 & di>0] = di[bi==0 & fi==0 & di>0] - 1

	di0         = which(di==0)
	dg          = di
	dg[di==0 & li>0]  = li[di==0 & li>0] + 1
	dg[di==0 & li==0] = bi[di==0 & li==0] + 1
	dg[dg<Ti] = Ti+1

	xg          = dg - bg

	# Full observation matrix:
	Fg          = c(apply(cbind(Ti, bg+1), 1, max))
	Lg          = c(apply(cbind(Tf, dg-1), 1, min))
	Og          = ObsMatFun(Fg, Lg, Tm)
	fii         = fi; fii[bi>0 & bi>=Ti] = bi[bi>0 & bi>=Ti]+1; fii[bi>0 & bi<Ti] = Ti
	lii         = li; lii[di>0 & di<=Tf] = di[di>0 & di<=Tf]-1; lii[di>0 & di>Tf] = Tf
	lfi         = ObsMatFun(fii, lii, Tm)

	# Test initial parameters and jumps:
	# Test initial values:
	idtrg       = which(bg<Ti)
	p.xm        = sum(fx.fun(xg + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE)) - 
	              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthg[idtrg,], Zgag[idtrg], idm=idm, log=TRUE))
	
	if(is.na(p.xm) | p.xm == -Inf) ModifyPars = TRUE else ModifyPars = FALSE
	

	# Test lower bound:
	thnl        = matrix(qtnorm(rep(c(0.999, 0.001, 0.999,0.999, 0.999),each=nza), 
	              mean=c(thg), sd=c(thj), lower=c(low)), nza, nth) * modm[rep(idm,nza), ]
	if(Cont) ganl = qnorm(rep(0.001, nzc), mean=gag, sd=gaj) else ganl = gag

	Zthnl       = Za %*% thnl
	Zganl       = Zc %*% ganl
	
	p.xl        = sum(fx.fun(xg + 0.5*Dx, Zthnl, Zganl, idm=idm, log=TRUE)) - 
	              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthnl[idtrg,], Zganl[idtrg], idm=idm, log=TRUE))
	
	# Test upper bound:
	thnu        = matrix(qtnorm(rep(c(0.001, 0.999, 0.001, 0.001, 0.001),each=nza), 
	              mean=c(thg), sd=c(thj), lower=c(low)), nza, nth) * modm[rep(idm,nza), ]

	if(Cont) ganu = qnorm(rep(0.999, nzc), mean=gag, sd=gaj) else ganu = gag

	Zthnu       = Za %*% thnu
	Zganu       = Zc %*% ganu
	
	p.xu        = sum(fx.fun(xg + 0.5*Dx, Zthnu, Zganu, idm=idm, log=TRUE)) - 
	              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthnu[idtrg,], Zganu[idtrg], idm=idm, log=TRUE))
	
	if(is.na(p.xl) | p.xl == -Inf | is.na(p.xu) | p.xu == Inf) ModifyJumps = TRUE else ModifyJumps = FALSE


	if(!ModifyPars & !ModifyJumps){
		runmcmc     = TRUE

		# Output tables:
		thing       = seq(bng, ng, by=thint)
		thgibbs     = matrix(NA,ng,length(idth))
		colnames(thgibbs) = pname[idth]
		gagibbs     = matrix(NA, ng, nzc)
		colnames(gagibbs) = colnames(Zc)
		pigibbs     = matrix(NA, ng, npi)
		colnames(pigibbs) = paste("pi.", diffrec,sep="")
		bgibbs      = matrix(NA,length(thing),n)
		dgibbs      = bgibbs
		postm       = matrix(NA, ng, 3)
		colnames(postm) = c("post.th", "post.X", "full.post")

		# Run Gibbs sampler:
#		if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
#		devtype(width=2, height=0.5); progrpl = dev.cur()
		if(!exists("Ypos")) Ypos = 0
#		Ypos       = get(Ypos, pos=sys.frame(which=1))
		if(!exists("sim")) Title = "Sim.1" else Title = paste("Sim.", sim, sep="")
		x11(width=2, height=0.5, ypos = Ypos, xpos=0, title = Title); progrpl = dev.cur()
		par(mar=rep(0,4))

		naflag     = FALSE
		Start      = Sys.time()
		g          = 1
		gg         = 1
		while(g <= ng & !naflag){
			if(g==1){cat("MCMC is running...\n")}
		
			# 1.- SAMPLING:
			# a) Sample survival parameters:
			thn         = thg * 0
			thn[idth]   = c(rtnorm(length(idth), thg[idth], thj[idth], lower=low[idth]))
			if(idm>1){
				low[,3]     = apply(thn, 1, c.low, idm=idm)
				idcl        = which(thn[,3] < low[,3])
				if(length(idcl)>0){
					for(cc in idcl) thn[cc,3]   = c(rtnorm(1, thg[cc,3], thj[cc,3], lower=low[cc,3]))
				}
			}
			if(Cont) gan = rnorm(nzc, gag, gaj) else gan = gag
			

			Zthn        = Za %*% thn
			Zgan        = Zc %*% gan
			idtrg       = which(bg<Ti)	

			p.thg       = sum(fx.fun(xg + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE)) - 
			              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthg[idtrg,], Zgag[idtrg], idm=idm, log=TRUE)) + 
			              sum(dtnorm(c(thg), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gag, gap, gav, log=TRUE))

			p.thn       = sum(fx.fun(xg + 0.5*Dx, Zthn, Zgan, idm=idm, log=TRUE)) - 
			              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthn[idtrg,], Zgan[idtrg], idm=idm, log=TRUE)) + 
			              sum(dtnorm(c(thn), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gan, gap, gav, log=TRUE))

			r           = exp(p.thn-p.thg)
			z           = runif(1,0,1)

			if(is.na(r) & g==1){
				dev.off(progrpl)
				naflag     = TRUE 
			} else if(is.na(r) & g > 1){
				dev.off(progrpl)
				naflag     = TRUE 
			} else {
				if(r>z){
					thg   = thn
					Zthg  = Zthn
					p.thg = p.thn
					gag   = gan
					Zgag  = Zgan
				}
			}

			# b) Sample times of birth and death:
			bn          = bg 
			bn[bi0]     = bg[bi0] + sample(-1:1, length(bi0), replace=TRUE) 
			bn[bi0]     = apply(cbind(bn[bi0],fi[bi0]-1),1,min)

			idtrn       = which(bn<Ti)
			dn          = dg 
			dn[di0]     = dg[di0] + sample(-1:1, length(di0), replace=TRUE) 
			dn[di0]     = apply(cbind(dn[di0],bn[di0],li[di0]+1),1,max) 

			xn          = dn - bn
 
			Fn          = c(apply(cbind(Ti, bn+1), 1, max))
			Ln          = c(apply(cbind(Tf, dn-1), 1, min))
			On          = ObsMatFun(Fn, Ln, Tm)
    
			p.bdg       = fx.fun(xg + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE) + 
			              (Og - lfi) %*% log(1-Pig) +
			              log(v.x(xg + 0.5*Dx))

			p.bdn       = fx.fun(xn + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE) + 
			              (On - lfi) %*% log(1-Pig) +
			              log(v.x(xn + 0.5*Dx))
	
			r           = exp(p.bdn-p.bdg)
			if(length(which(is.na(r)))>0){
				dev.off(progrpl) 
				naflag      = TRUE 
			} else {
				z           = runif(n, 0, 1)
				bg[r>z]     = bn[r>z]
				dg[r>z]     = dn[r>z]
				xg[r>z]     = xn[r>z]
				p.bdg[r>z]  = p.bdn[r>z]
				Og[r>z,]    = On[r>z,]
			}

			# c) Sample recapture probability:
			rho1g       = rho1 + t(t(Y)%*% rep(1,n))
			rho2g       = rho2 + t(t(Og - Y)%*% rep(1,n))
			Rho1        = tapply(rho1g, idpi, sum)
			Rho2        = tapply(rho2g, idpi, sum)
			pig         = rbeta(npi, Rho1, Rho2)
			if(1 %in% pig){
				pig[pig==1] = 1-1e-5
				warning("Some recapture probabilities are equal to 1\nThey have been constraint to be fractionally less than 1 for computational reasons\n", call.=FALSE)
			} 
			Pig         = pig[idpi]
		
			# 2.- STORE RESULTS:
			# Parameters and latent states:
			thgibbs[g,] = thg[idth]
			pigibbs[g,] = pig
			if(Cont){
				gagibbs[g,]  = gag
			}
			if(g %in% thing){
				bgibbs[gg,]  = bg
				dgibbs[gg,]  = dg
				gg           = gg + 1
			}

			# Conditional posteriors:
			postm[g,]   = c(p.thg, sum(p.bdg), p.thg + sum((Og - lfi) %*% log(1-Pig) + log(v.x(xg + 0.5*Dx))))
  
			# Progress plot:
			if(g %in% round(seq(1,ng,length=100))){
				par(mar=rep(0,4))
				plot(c(0,ng*1.1), c(0,1), axes=FALSE, col=NA, xlab="", ylab="")
				polygon(c(0,ng,ng,0), c(0.35,0.35,0.65,0.65), col=NA, border='dark red')
				polygon(c(0,g,g,0), c(0.35,0.35,0.65,0.65), col='dark red', border='dark red')
				text(ng/2, 0.85, "MCMC progress", cex=0.9)
				text(g, 0.15, paste(round(g/ng*100), "%", sep=""), cex=0.8)
			}
			g          = g+1
		}
		if(g == ng+1) g = ng
		if(!naflag) cat("MCMC finished running\n") else cat(paste("MCMC stopped at step ", g,"\nPdf of ages at death equal to 0 for some individuals.\nModify starting parameters or jumps\n", sep=""))
		dev.off(progrpl)
	
		# RESULTS SUMMARY:
		if(!naflag){

			# Model selection diagnostics:
			# Calculate DIC
			modepost   = postm[thing,3]
			L          = length(thing)
			Dm         = -2*modepost
			Dmode      = -2*modepost[which(modepost==max(modepost))[1]]
			Dave       = mean(Dm)
			pD         = Dave - Dmode	
			k          = npi + sum(modm[idm,])*nza
			DIC        = 2*Dave - Dmode
			ModSel     = c(Dave, Dmode, pD, k, DIC)
			names(ModSel) = c("D.ave", "D.mode", "pD", "k", "DIC")
			full.run  = TRUE
		}
			
	} else {
		if(ModifyPars) cat("\nInitial parameters produce pdf values equal to 0 for some individuals.\nModify initial parameters and run model again.\n")
		if(ModifyJumps) cat("\nJumps produce pdf values equal to 0 for some individuals.\nReduce jumps and run model again.\n")
		runmcmc     = FALSE
		naflag      = TRUE
		thgibbs     = matrix(NA,length(1:niter),length(idth))
		colnames(thgibbs) = pname[idth]
		gagibbs     = matrix(NA, length(1:niter), length(nzc))
		colnames(gagibbs) = colnames(Zc)
		pigibbs     = matrix(NA, length(1:niter), length(rptp))
		colnames(pigibbs) = paste("pi.", diffrec,sep="")
		bgibbs      = matrix(NA,length(seq(burnin, niter, thinning)),n)
		dgibbs      = bgibbs
		postm       = matrix(NA, length(1:niter), 3)
		colnames(postm) = c("post.th", "post.X", "full.post")
		full.run    = FALSE
		g           = 0
	}

	if(naflag) {
		full.run  = FALSE
		ModSel     = rep(NA, 5)
		names(ModSel) = c("D.ave", "D.mode", "pD", "k", "DIC")
	}
	
	#Return a list object
	output          = list()
	output$data     = list(bd = bd,Y = Y,Za = Za,Zc = Zc, ststart=ststart, stend=stend)
	output$input    = list(niter=niter, burnin = burnin, thinning = thinning, model=model, modm=modm, idm=idm, ini.pars=ini.pars, jumps=jumps, priors=priors, Prop.Hazards=Prop.Hazards, cini.pars=cini.pars, cjumps=cjumps, cpriors=cpriors)
	output$results  = list(theta=thgibbs, gamma=gagibbs, pi = pigibbs, bis = bgibbs, xis = dgibbs-bgibbs, post=postm)
	output$diagnost = list(full.run=full.run, last.step=g, ModSel = ModSel)
	return(output)
}

