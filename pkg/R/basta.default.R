basta.default <-
function(object, ststart, stend, model="SI", niter=50000, burnin=5001, thinning=50, rptp = ststart, th.ini.pars=NULL, th.jumps=NULL, th.priors=NULL, Prop.Hazards = FALSE, ga.ini.pars=NULL, ga.jumps=NULL, ga.priors=NULL, nsim=1, parallel=FALSE, ncpus=2, lifetable=TRUE, progr.plot=FALSE, ...){

	# 0. Load package msm:
	require(msm)

	# 1. object error checking:
	tempcheck   = DataCheck(object, ststart, stend, silent=TRUE)
	if(tempcheck[[1]] == FALSE) stop("You have an error in Dataframe 'object',\nplease use function 'DataCheck'\n", call.=FALSE)

    # 2. Check that niter, burnin, and thinning are compatible.
    if(burnin>niter) stop("\nObject 'burnin' larger than 'niter'.")
    if(thinning>niter) stop("\nObject 'thinning' larger than 'niter'.")
    
    # 3. Functions:
    # 3.1 Survival, mort, pdf:
    Sx.fun <- function(x,th, ga=0, idm=3, log=FALSE){
		Sx    =  x * (-th[,3]) + exp(th[,4])/th[,5] * (1-exp(th[,5]*x))
		if(idm==3) Sx = Sx + (exp(th[,1])/th[,2] * (exp(-th[,2]*x)-1))
		Sx    = Sx*exp(ga)
		if(!log) Sx = exp(Sx)
		return(Sx)
	}

	mx.fun <- function(x,th, ga=0, idm=3, log=FALSE){
		mx    = th[,3] + exp(th[,4] + th[,5]*x)
		if(idm==3) mx = mx + exp(th[,1]-th[,2]*x)
		mx    = mx * exp(ga)
		if(log) mx = log(mx)
		return(mx)
	}

    fx.fun <- function(x,th, ga=0, idm=3, log=FALSE){
		fx       = mx.fun(x, th, ga, idm=idm, log=TRUE) + Sx.fun(x, th, ga, idm=idm, log=TRUE)
		if(!log) fx = exp(fx)
		return(fx)
	}

	S.x         = function(th) Sx.fun(xv, matrix(th,1,nth), gaa, idm=idm)
	m.x         = function(th) mx.fun(xv, matrix(th,1,nth), gaa, idm=idm)

	# 3.2 object processing:
	Ith.fun <- function(Z){
		lu          = apply(Z, 2, function(x) length(unique(x)))
		ru          = apply(Z, 2, range)
		idcat       = which(lu==2 & apply(ru,2,sum)==1)
		if(length(idcat)==0) idcat = NULL
		idint       = which(lu==1)
		if(length(idint)==0) idint = NULL
		idcon       = which(lu>2)
		if(length(idcon)==0) idcon = NULL
	
		return(list(int=idint, cat=idcat, cont=idcon))
	}

	ObsMatFun <- function(f, l, Tm){
		Fm    = Tm - f; Fm[Fm>=0] =  1; Fm[Fm<0] = 0
		Lm    = Tm - l; Lm[Lm<=0] = -1; Lm[Lm>0] = 0
		return(Fm * (-Lm))	
	}

	c.low <- function(th, idm=3){
		if(idm==1) cl = 0
		if(idm==2) cl = ifelse(th[5] > 0, -exp(th[4]), 0)
		if(idm==3){
			x.minf = (th[1]+log(th[2]) - th[4]-log(th[5]))/(th[2] + th[5])
			cl     = -exp(th[1]-th[2]*(x.minf)) - exp(th[4]+th[5]*(x.minf))
		}
		return(cl)
	}

	TestParsJumps  = function(th, ga, tj, gj, Za, Zc, xg, bg, Ti, idm, nza, nzc, Dx, low, nth, modm, Cont){
		Zthm        = Za %*% th
		Zgam        = Zc %*% ga

		idtrg       = which(bg<Ti)
		p.xm        = sum(fx.fun(xg + 0.5*Dx, Zthm, Zgam, idm=idm, log=TRUE)) - 
		              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthm[idtrg,], Zgam[idtrg], idm=idm, log=TRUE))
	
		if(is.na(p.xm) | p.xm == -Inf) ModifyPars = TRUE else ModifyPars = FALSE
	
		# 4.2 Test lower bound:
		thnl        = matrix(qtnorm(rep(c(0.999, 0.001, 0.999,0.999, 0.999),each=nza), 
		              mean=c(th), sd=c(tj), lower=c(low)), nza, nth) * modm[rep(idm,nza), ]
		if(Cont) ganl = qnorm(rep(0.001, nzc), mean=ga, sd=gj) else ganl = ga

		Zthnl       = Za %*% thnl
		Zganl       = Zc %*% ganl
	
		p.xl        = sum(fx.fun(xg + 0.5*Dx, Zthnl, Zganl, idm=idm, log=TRUE)) - 
		              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthnl[idtrg,], Zganl[idtrg], idm=idm, log=TRUE))
	
		# 4.3 Test upper bound:
		thnu        = matrix(qtnorm(rep(c(0.001, 0.999, 0.001, 0.001, 0.001),each=nza), 
		              mean=c(th), sd=c(tj), lower=c(low)), nza, nth) * modm[rep(idm,nza), ]

		if(Cont) ganu = qnorm(rep(0.999, nzc), mean=ga, sd=gj) else ganu = ga

		Zthnu       = Za %*% thnu
		Zganu       = Zc %*% ganu
	
		p.xu        = sum(fx.fun(xg + 0.5*Dx, Zthnu, Zganu, idm=idm, log=TRUE)) - 
		              sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, Zthnu[idtrg,], Zganu[idtrg], idm=idm, log=TRUE))
	
		if(is.na(p.xl) | p.xl == -Inf | is.na(p.xu) | p.xu == Inf) ModifyJumps = TRUE else ModifyJumps = FALSE

		return(list(pars=ModifyPars, jumps = ModifyJumps))

	}
    
    # 3. Model structure setup:
    # 3.1 Extract raw data:
	Ti          = ststart
	Tf          = stend
	st          = Ti:Tf
	nt          = length(st)
	idnames     = object[,1]
	n           = nrow(object)
	bd          = as.matrix(object[,2:3])
	Y           = as.matrix(object[,1:nt+3]); colnames(Y) = st

	paralvars   = c("Ti","Tf","st","nt","n","bd","Y","rptp","progr.plot") 

	# 3.2 Extract covariates:
	# a) Find if there are covariates:
	if(ncol(object)>nt+3){
		Z         = as.matrix(object[,(nt+4):ncol(object)])
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
				Zc     = matrix(Z[,Ith$cont], nrow(Z), length(Ith$cont))
				Zc     = t(t(Zc)-apply(Zc,2,mean))
				colnames(Zc) = colnames(object)[(nt+4):ncol(object)][Ith$cont]
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
					colnames(Za) = c("Intercept", colnames(Za)[-1])
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

	paralvars   = c(paralvars,"Za","Zc","Cont","nza","nzc") 

	# 3.3. Model Matrix and lower limits for parameters:
	nth         = 5
	modm        = matrix(c(0,0,1,0,0,1,0,rep(1,8)), 3, nth, dimnames=list(c("GO", "GM", "SI"), c("theta.a1", "theta.b1","theta.c","theta.a2","theta.b2")))
	thname      = paste(rep(colnames(modm),each=nza), "[",rep(colnames(Za), nth),"]", sep="")
	if(nza==1) thname = colnames(modm)
	idm         = which(rownames(modm)==model)
	idth        = which(modm[rep(idm, nza),]==1)
	idths       = which(modm[idm,]==1)
	nthm        = sum(modm[idm,])
	th.low      = matrix(-Inf, nrow(modm),nth, dimnames=dimnames(modm))
	th.low["SI",c("theta.b1","theta.b2")] = 0
	low         = matrix(th.low[idm,],nza, nth, byrow=TRUE)
	dimnames(low) = list(colnames(Za), colnames(modm))
	ganame      = paste("gamma[",colnames(Zc),"]",sep=""); if(nzc == 1) ganame = "gamma"
	piname      = ifelse(length(rptp)==1, "pi", paste("pi[", rptp,"]",sep=""))
	poname      = c("post[th,ga]", "post[X0]", "post[full]")
	Pname       = paste(rep(colnames(modm),each=nza), "[",rep(colnames(Za), nth),"]", sep="")
	if(nza==1)  Pname = colnames(modm)

	paralvars   = c(paralvars, "nth", "modm", "thname", "ganame", "piname", "poname", "idm", "idth", "low") 

	# 3.4 MCMC setup variables:
	ng          = niter
	bng         = burnin
	thint       = thinning

	paralvars   = c(paralvars, "ng", "bng", "thint") 

	# 3.5 Verify th.jumps, initial parameters and th.priors: 
	# 3.5.1 Survival model:
	# a) Initial parameters:
	if(is.null(th.ini.pars)){
		thg       = t(t(modm[rep(idm, nza),])*c(-1, 0.001, -0.05, -1, 0.001))
		if(nza==1) thg = t(thg)
	} else {
		lini.pars = length(th.ini.pars)
		if(lini.pars < nthm){
			stop(paste("\nLength of starting parameters (argument 'th.ini.pars') for the mortality model is less than ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(lini.pars > nthm) {
			stop(paste("\nLength of starting parameters (argument 'th.ini.pars') for the mortality model is larger than ", nthm , " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE)		
		}
		thg       = modm[rep(idm, nza), ]
		thg[,modm[idm,]==1] = matrix(t(th.ini.pars),nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thg = t(thg)
	}	
	thgini    = thg[,idths]

	# b) Jumps:
	if(is.null(th.jumps)){
		thj       = t(t(modm[rep(idm, nza),])*c(0.005, 0.005, 0.02, 0.0075, 0.001))
		if(nza==1) thj = t(thj)
	} else {
		ljumps    = length(th.jumps)
		if(ljumps < nthm){
			stop(paste("\nLength of th.jumps for the mortality model parameters is less than ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(ljumps > nthm) {
			stop(paste("\nLength of th.jumps for the mortality model parameters is larger than ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE)		}
		thj       = modm[rep(idm, nza), ]
		thj[,modm[idm,]==1] = matrix(t(th.jumps), nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thj = t(thj)
	}
	thjini    = thj[,idths]

	# c) Priors:
	if(is.null(th.priors)){
		thp       = t(t(modm[rep(idm, nza),])*c(-5,0.1,-0.5,0.001,0.001))
		if(nza==1) thp = t(thp)
	} else {
		lpriors   = length(th.priors)
		if(lpriors < nthm){
			stop(paste("\nLength of th.priors for the mortality model parameters is less than ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE) 
		} else if(lpriors > nthm) {
			stop(paste("\nLength of th.priors for the mortality model parameters is larger than ", nthm, " (i.e. number of parameters for model ", model,")", sep=""), call.=FALSE)		}
		thp       = modm[rep(idm, nza), ]
		thp[,modm[idm,]==1] = matrix(t(th.priors),nza, sum(modm[idm,]), byrow=TRUE)
		if(nza==1) thp = t(thp)
	}
	thpini    = thp[,idths]
	
	
	# 3.5.2 Proportional hazards section:
	if(Cont){
		
		# a) Initial parametes:
		if(is.null(ga.ini.pars)){
			gag     = rep(0, nzc)
			names(gag) = colnames(Zc)
		} else {
			lcini.pars = length(ga.ini.pars)
			if(lcini.pars != nzc){
				stop(paste("\nLength of prop. hazards parameters is not equal to number of covariates (n = ", nzc,").", sep=""), call.=FALSE)
			} else {
				gag    = ga.ini.pars
				names(gag) = colnames(Zc)
			}
		}
		gagini   = gag
		
		# b) Jumps:
		if(is.null(ga.jumps)){
			gaj     = rep(0.01, nzc)
			names(gaj) = colnames(Zc)
		} else {
			lcjumps = length(ga.jumps)
			if(lcjumps != nzc){
				stop(paste("\nLength of th.jumps for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
			} else {
				gaj    = ga.jumps
				names(gaj) = colnames(Zc)
			} 
		}
		
		# c) Priors:
		if(is.null(ga.priors)){
			gap     = rep(0, nzc)
			names(gap) = colnames(Zc)
		} else {
			lcpriors = length(ga.priors)
			if(lcpriors != nzc){
				stop(paste("\nLength of th.priors for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
			} else {
				gap    = ga.jumps
				names(gap) = colnames(Zc)
			} 
		}
		
	} else {
		gag    = 0
		gagini = gag
		gaj    = 0
		gap    = 0
	}


	# 3.6 Model variables:
	# 3.6.1 Extract times of birth and death:
	bi          = bd[,1]
	di          = bd[,2]

	# 3.6.2 Define study duration:
	Dx          = (st[2]-st[1])
	Tm          = matrix(st, n, nt, byrow=TRUE)

	# 3.6.3 Calculate first and last time observed:
	ytemp       = t(t(Y) * st)
	li          = c(apply(ytemp,1,max))
	ytemp[ytemp==0] = 10000
	fi          = c(apply(ytemp,1,min))
	fi[fi==10000] = 0
	rm("ytemp")

	# 3.6.4 Calculate number of times detected:
	oi          = Y %*% rep(1, nt)

	paralvars   = c(paralvars, "bi", "di", "Dx", "Tm", "li", "fi") 

	# 3.6.5 Priors:
	# a) Survival parameters:
	Zthp        = Za %*% thp
	thv         = 0.5
		
	# b) Prop. hazards section:
	Zgap        = Zc %*% gap
	gav         = 1

	# c) Age distribution:
	dxx         = 0.001
	xx          = seq(0,100,dxx)
	zza         = cbind(1,matrix(0, length(xx), nza-1))
	zzc         = sum(apply(Zc, 2, mean) * gap) 
	Ex          = sum(xx*fx.fun(xx,zza %*% thp, zzc, idm=idm)*dxx)
	v.x         = function(x) Sx.fun(x,Zthp, Zgap, idm=idm)/Ex

	# d) Detection probability:
	idpi        = findInterval(st, rptp); names(idpi) = st
	npi         = length(unique(idpi))
	rho1        = 0.1
	rho2        = 0.1

	paralvars   = c(paralvars, "Zthp", "thv", "Zgap", "gav", "Ex", "v.x", "idpi", "npi", "rho1", "rho2") 

	# 3.6.6 Starting values:
	# a) Survival parameters
	Zthg        = Za %*% thg
		
	# b) Prop. hazards parameter:
	Zgag        = Zc %*% gag

	# c) Recapture probability:
	pig         = rep(0.5, npi)
	Pig         = pig[idpi]

	# d) Times of birth and death:
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

	paralvars   = c(paralvars, "thg", "thj", "thp", "gag", "gaj", "gap", "pig", "Pig", "bg", "dg", "xg", "bi0", "di0") 

	# e) Full observation matrix:
	Fg          = c(apply(cbind(Ti, bg+1), 1, max))
	Lg          = c(apply(cbind(Tf, dg-1), 1, min))
	Og          = ObsMatFun(Fg, Lg, Tm)
	fii         = fi; fii[bi>0 & bi>=Ti] = bi[bi>0 & bi>=Ti]+1; fii[bi>0 & bi<Ti] = Ti
	lii         = li; lii[di>0 & di<=Tf] = di[di>0 & di<=Tf]-1; lii[di>0 & di>Tf] = Tf
	lfi         = ObsMatFun(fii, lii, Tm)

	paralvars   = c(paralvars, "Fg", "Lg", "Og", "lfi") 

	# 4. Test initial parameters and th.jumps:
	TestIniPJ   = TestParsJumps(thg, gag, thj, gaj, Za, Zc,xg, bg, Ti, idm, nza, nzc, Dx, low, nth, modm, Cont)

	if(TestIniPJ$p) stop("\nInitial parameters produce pdf values equal to 0 for some individuals.\nModify initial parameters and run model again.\n", call.=FALSE)
	if(TestIniPJ$j) stop("\nJumps produce pdf values equal to 0 for some individuals.\nReduce jumps and run model again.\n", call.=FALSE)

	# 5.  Multiple MCMC function:
	multiMCMC  = function(sim){
		if(parallel) for(ii in 1:(sim*2)){}
		if(nsim > 1){
			inipj     = FALSE
			while(!inipj){
				thn         = thg * 0
				thn[idth]   = c(rtnorm(length(idth), thg[idth], 0.25, lower=low[idth]))
				if(idm>1){
					low[,3]     = apply(thn, 1, c.low, idm=idm)
					idcl        = which(thn[,3] < low[,3])
					if(length(idcl)>0){
						for(cc in idcl) thn[cc,3]   = c(rtnorm(1, thg[cc,3], 0.5, lower=low[cc,3]))
					}
				}
				if(Cont) gan = rnorm(nzc, gag, 0.5) else gan = gag
				testpj    = TestParsJumps(thn, gan, thj, gaj, Za, Zc,xg, bg, Ti, idm, nza, nzc, Dx, low, nth, modm, Cont)
				inipj     = ifelse(testpj$p & testpj$j, FALSE, TRUE)
			}
			thg         = thn
			if(Cont) gag  = gan
		}
	
		# Output tables:
		thing       = seq(bng, ng, by=thint)
		thgibbs     = matrix(NA,ng,length(idth))
		colnames(thgibbs) = thname[idth]
		gagibbs     = matrix(0, ng, nzc)
		colnames(gagibbs) = ganame
		pigibbs     = matrix(NA, ng, npi)
		colnames(pigibbs) = piname
		bgibbs      = matrix(NA,length(thing),n)
		dgibbs      = bgibbs
		postm       = matrix(NA, ng, 3)
		colnames(postm) = poname
		thgibbs[1,] = thg[idth]
		pigibbs[1,] = pig
		if(Cont){
			gagibbs[1,]  = gag
		}
		Zthg        = Za %*% thg
		Zgag        = Zc %*% gag

		# Run Gibbs sampler:
		naflag      = FALSE
		g           = 2
		gg          = 1
		rownames(thg) = rownames(modm[rep(idm, nza), ])
		if(progr.plot){
			if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
			devtype(width=2, height=0.5); progrpl = dev.cur()
			par(mar=rep(0,4))
		}
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

			p.thg       = sum(fx.fun(xg + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE)) + sum(dtnorm(c(thg), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gag, gap, gav, log=TRUE))

			p.thn       = sum(fx.fun(xg + 0.5*Dx, Zthn, Zgan, idm=idm, log=TRUE)) + sum(dtnorm(c(thn), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gan, gap, gav, log=TRUE))

			if(length(idtrg)>0){
				p.thg      = p.thg - sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, matrix(Zthg[idtrg,], length(idtrg), ncol(Zthg)), matrix(Zgag[idtrg], length(idtrg), ncol(Zthg)), idm=idm, log=TRUE)) 
				
				p.thn      = p.thn - sum(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, matrix(Zthn[idtrg,], length(idtrg), ncol(Zthg)), matrix(Zgan[idtrg], length(idtrg), ncol(Zthg)), idm=idm, log=TRUE)) 
			}


			r           = exp(p.thn-p.thg)
			z           = runif(1,0,1)

			if(is.na(r) & g==1){
				if(progr.plot) dev.off(progrpl)
				naflag     = TRUE 
			} else if(is.na(r) & g > 1){
				if(progr.plot) dev.off(progrpl)
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

			dn          = dg 
			dn[di0]     = dg[di0] + sample(-1:1, length(di0), replace=TRUE) 
			dn[di0]     = apply(cbind(dn[di0],bn[di0],li[di0]+1),1,max) 

			xn          = dn - bn

			Fn          = c(apply(cbind(Ti, bn+1), 1, max))
			Ln          = c(apply(cbind(Tf, dn-1), 1, min))
			On          = ObsMatFun(Fn, Ln, Tm)
    
			p.bdg       = fx.fun(xg + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE) + (Og - lfi) %*% log(1-Pig) + log(v.x(xg + 0.5*Dx))

			p.bdn       = fx.fun(xn + 0.5*Dx, Zthg, Zgag, idm=idm, log=TRUE) + (On - lfi) %*% log(1-Pig) + log(v.x(xn + 0.5*Dx))
	
			r           = exp(p.bdn-p.bdg)
			if(length(which(is.na(r)))>0){
				if(progr.plot) dev.off(progrpl) 
				naflag      = TRUE 
			} else {
				z           = runif(n, 0, 1)
				bg[r>z]     = bn[r>z]
				dg[r>z]     = dn[r>z]
				xg[r>z]     = xn[r>z]
				p.bdg[r>z]  = p.bdn[r>z]
				Og[r>z,]    = On[r>z,]
			}

			# c) Sample recapture probability(ies):
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
			if(g %in% round(seq(1,ng,length=100)) & progr.plot){
				par(mar=rep(0,4))
				plot(c(0,ng*1.1), c(0,1), axes=FALSE, col=NA, xlab="", ylab="")
				polygon(c(0,ng,ng,0), c(0.35,0.35,0.65,0.65), col=NA, border='dark red')
				polygon(c(0,g,g,0), c(0.35,0.35,0.65,0.65), col='dark red', border='dark red')
				text(ng/2, 0.85, paste("MCMC progress (Sim. ",sim,")", sep=""), cex=0.9)
				text(g, 0.15, paste(round(g/ng*100), "%", sep=""), cex=0.8)
			}
			g          = g+1
		}
		if(g == ng+1) g = ng
		if(progr.plot) dev.off(progrpl)
		
		# Calculate MCMC diagnostics:
		return(list(theta=thgibbs, gamma=gagibbs, pi=pigibbs, bi=bgibbs, di=dgibbs, post=postm, g=g, naflag=naflag))
	}


	# 6. Run (multi) MCMC:
	if(nsim==1){
		parallel = FALSE
	}
	if(nsim > 1) cat("Multiple simulations started...\n\n") else cat("Simulation started...\n\n")
	Start      = Sys.time()
	if(parallel){
		availpkg     = available.packages()
		if(!is.element("snowfall", availpkg)){
			warning("\nPackage 'snowfall' is not installed.\nSimulations will not be ran in parallel (computing time will be longer...)\n")
			object   = lapply(1:nsim, multiMCMC)
		} else {
			require(snowfall)
			sfInit(parallel=TRUE, cpus=ncpus);
			sfExport(list=c(paralvars, "parallel", "fx.fun", "mx.fun", "Sx.fun", "ObsMatFun", "c.low", "TestParsJumps", "nsim"))
			sfLibrary(msm)
			object   = sfClusterApplyLB(1:nsim, multiMCMC)
			sfStop()
		}
	} else {
		object   = lapply(1:nsim, multiMCMC)
	}
	End        = Sys.time()


	# Report if all simulations ran through:
	simname    = paste("Sim.", (1:nsim), sep="")
	names(object) = simname
	full.runs  = rep(0,nsim); names(full.runs) = simname
	last.steps = full.runs
	for(i in 1:nsim){
		last.steps[i] = object[[i]]$g		
		full.runs[i]  = ifelse(last.steps[i]==ng, 1, 0)
	} 
	id.failed  = which(full.runs==0)
	all.ran    = FALSE
	if(nsim==1){
		if(full.runs==1){
			cat("MCMC finished running\n")
			cat(paste("Total MCMC computing time: ", round(as.numeric(julian(End)-julian(Start))*24*60, 2), " minutes\n\n", sep=""))
			all.ran = TRUE
		} else {
			cat(paste("MCMC stopped at step ", object[[1]]$g,"\nPdf of ages at death equal to 0 for some individuals.\nModify starting parameters or jumps\n", sep=""))
		}
	} else {
	if(length(id.failed)>0 & length(id.failed)<nsim){
		cat("\nOne or more simulations failed\nConvergence diagnostics and model selection will not be calculated.\n")
	} else if(length(id.failed)==nsim){
		cat("\nAll simulations failed\nConvergence diagnostics and model selection will not be calculated.\n")
	} else {
		all.ran    = TRUE
		cat("\nMultiple simulations finished.\n")
		cat(paste("Total MCMC computing time: ", round(as.numeric(julian(End)-julian(Start))*24*60, 2), " minutes\n\n", sep=""))
		}
	}	

	# 7. Diagnostics:
	thing      = seq(burnin, niter, thinning)
	nthin      = length(thing)

	# 7.1 Thinned result matrices:
	if(Cont) pname  = c(thname[idth], ganame, piname, poname) else pname = c(thname[idth], piname, poname)
	Pmat       = matrix(NA, ng*nsim, length(pname))
	dimnames(Pmat) = list(rep(simname, each=ng), pname)
	Bimat      = matrix(NA, nthin*nsim, n)
	rownames(Bimat) = rep(simname, each=nthin)
	Dimat      = Bimat
	idthin     = rep(0, ng*nsim)

	for(i in 1:nsim){
		Idsim       = which(rownames(Pmat)==simname[i])
		if(Cont){
			Pmat[Idsim,] = cbind(object[[i]]$theta, object[[i]]$gamma, object[[i]]$pi, object[[i]]$post)
		} else {
			Pmat[Idsim,] = cbind(object[[i]]$theta, object[[i]]$pi, object[[i]]$post)
		}
		idthin[Idsim[thing]] = 1
		Idsim       = which(rownames(Bimat)==simname[i])
		Bimat[Idsim,] = object[[i]]$bi
		Dimat[Idsim,] = object[[i]]$di
	}
		
	# 7.2 Basic summary statistics for parameters:
	pmat      = Pmat[idthin==1,-(c(ncol(Pmat)-c(2:0)))]
	coef      = cbind(apply(pmat, 2, mean, na.rm=TRUE), apply(pmat, 2, sd, na.rm=TRUE), t(apply(pmat, 2, quantile, c(0.025, 0.975), na.rm=TRUE)), NA, NA, NA)
	colnames(coef) = c("Estimate", "StdErr", "Lower95%CI", "Upper95%CI", "SerAutocor", "UpdateRate", "PotScaleReduc")
	if(length(id.failed) < nsim){
		coef[,"SerAutocor"]  = apply(pmat,2, function(x) cor(x[-1],x[-length(x)], use="complete.obs"))
		coef[,"UpdateRate"]  = apply(Pmat[,-c(ncol(Pmat)-c(2:0))], 2, function(x) length(which(diff(x[!is.na(x)])!=0))/length(x[!is.na(x)]))
	} 
	
	Pmat      = cbind(idthin, Pmat)

	# 7.3 Convergence and model selection:
	if(all.ran){
		
		# If multiple simulations...
		if(nsim>1){

			# 7.3.1 Convergence diagnostics (potential scale reduction):
			Means       = apply(pmat, 2, function(x) tapply(x,rownames(pmat),mean))
			Vars        = apply(pmat, 2, function(x) tapply(x,rownames(pmat),var))
			meanall     = apply(Means,2,mean)
			B           = nthin/(nsim-1)*apply(t((t(Means)-meanall)^2),2,sum)
			W           = 1/nsim*apply(Vars,2,sum)
			Varpl       = (nthin-1)/nthin * W + 1/nthin*B
			Rhat        = sqrt(Varpl/W)
			conv        = cbind(B,W,Varpl,Rhat)
			rownames(conv) = colnames(pmat)
			coef[,ncol(coef)] = conv[,'Rhat']

			# Report if convergence was reached:
			idnconv     = which(conv[,'Rhat']< 0.95 | conv[,'Rhat']>1.2)
			if(length(idnconv)>0){
				ModSel     = NULL
				warning("Convergence not reached for some survival parameters.\nDIC could not be calculated.\n", call.=FALSE)

			} else {
				# 7.3.2 Model selection (DIC, if convergence was reached):
				modepost   = Pmat[idthin==1,ncol(Pmat)]
				L          = length(modepost)
				Dm         = -2*modepost
				Dmode      = -2*modepost[which(modepost==max(modepost))[1]]
				Dave       = mean(Dm)
				pD         = Dave - Dmode	
				k          = npi + length(idth)
				if(Cont) k = k + nzc
				DIC        = 2*Dave - Dmode
				ModSel     = c(Dave, Dmode, pD, k, DIC)
				names(ModSel) = c("D.ave", "D.mode", "pD", "k", "DIC")
				cat("Survival parameters converged appropriately.\nDIC was calculated\n.")
			}
		} else {
			conv       = NULL
			ModSel     = NULL
		}

		# 7.3.3 Summary times of birth and ages at death:
		xq        = apply(Dimat - Bimat, 2, quantile, c(0.5, 0.025, 0.975))
		bq        = apply(Bimat, 2, quantile, c(0.5, 0.025, 0.975))

		# 7.3.4 Summary Survival and mortality:
		thmat      = matrix(0, length(thing)*nsim, nth * nza); colnames(thmat) = Pname
		thmat[,thname[idth]] = pmat[,thname[idth]]
		if(Cont){
			rzc       = apply(Zc, 2, quantile, c(0.5, 0.025, 0.975))
			gave      = apply(as.matrix(pmat[,which(substr(colnames(pmat),1,2)=="ga")]),2,mean)
			zcname    = paste("Cont.",c("Med.","Lower","Upper")[i],sep="")
		} else {
			rzc       = matrix(0,1,1)
			gave      = 0
			zcname    = c("")
		}
		Sxq       = list()
		mxq       = list()
		xvec      = list()
		zaname    = colnames(Za)
		for(i in 1:nza){
			idza    = which(Za[,i]==1)
			xv      = seq(0,ceiling(max(xq[1,idza])*1.1),0.1)
			xvec[[zaname[i]]] = xv
			for(j in 1:nrow(rzc)){
				gaa    = sum(gave * rzc[j,])
				Cols   = paste(colnames(modm), "[",zaname[i],"]", sep="")
				if(nza==1) Cols = colnames(modm)
				Sxq[[zaname[i]]][[zcname[j]]] = apply(apply(thmat[,Cols],1,S.x),1, quantile, c(0.5,0.025,0.975))
				
				mxq[[zaname[i]]][[zcname[j]]] = apply(apply(thmat[,Cols],1,m.x),1, quantile, c(0.5,0.025,0.975))
			}
		}

		# 7.3.5 Calculate life table from estimated ages at death:
		if(lifetable){
			LT     = list()
			for(i in 1:nza){
				idza    = which(Za[,i]==1 & bq[1,]>=ststart)
				x       = xq[1,idza]
				LT[[zaname[i]]] = CohortLT(x, ax=0.5, n=1)
			}
		} else {
			LT     = NULL
		}
	} else {
		conv       = NULL
		ModSel     = NULL
		xq         = NULL
		bq         = NULL
		Sxq        = NULL
		mxq        = NULL
		xvec       = NULL
		if(lifetable) LT = NULL
	}

	
	# Return a list object:
	Settings           = c(niter, burnin, thinning, nsim)
	names(Settings)    = c("niter", "burnin", "thinning", "nsim") 
	ModelSpecs         = c(model, Prop.Hazards)
	names(ModelSpecs)  = c("model","Prop.Hazards")
	JumpPriors         = cbind(c(thj[idth],gaj), c(thp[idth],gap))
	dimnames(JumpPriors) = list(c(thname[idth],ganame), c("Jump.sd", "Mean.priors"))
	if(!Cont) JumpPriors = JumpPriors[-nrow(JumpPriors), ]
	output             = list()
	output$coefficients= coef
	output$ModSel      = ModSel
	output$Convergence = conv
	output$settings    = Settings
	output$ModelSpecs  = ModelSpecs
	output$JumpPriors  = JumpPriors
	output$Params      = Pmat
	output$Bis         = Bimat
	output$Dis         = Dimat
	output$Bq          = bq
	output$Xq          = xq
	output$Sx          = Sxq
	output$mx          = mxq
	output$xv          = xvec
	output$bd          = bd
	output$Y           = Y
	output$Za          = Za
	output$Zc          = Zc
	output$ststart     = ststart
	output$stend       = stend
	output$finished    = full.runs
	if(lifetable) output$lifetable = LT
	class(output) = "basta"
	return(output)
}
