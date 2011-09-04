basta.default <-
function(object, ststart, stend, model="GO", Shape="simple", covar.str = "mixed", niter=50000, burnin=5001, thinning=50, rptp = ststart, th.ini.pars=NULL, th.jumps=NULL, th.priors=NULL, ga.ini.pars=NULL, ga.jumps=NULL, ga.priors=NULL, nsim=1, parallel=FALSE, ncpus=2, lifetable=TRUE, progr.plot=FALSE, ...){

	# 1. Load package msm:
	require(msm)

    # 2. Functions:
    # 2.1 Survival, mort, pdf:
	if(model=="EX"){
		mx.fA     = function(x, th) th
		Sx.fA     = function(x, th) exp(- th * x )
		nthA      = 1
		lowA      = -Inf
		th.in     = 0.01
		jp.in     = 0.005
		pr.in     = 0.01
	} else if(model=="GO"){
		mx.fA     = function(x, th) exp(th[,1] + th[,2]*x )
		Sx.fA     = function(x, th) exp(exp(th[,1])/th[,2] * (1 - exp(th[,2]*x)) )
		nthA      = 2
		lowA      = c(-Inf, -Inf)
		th.in     = c(-1, 0.001)
		jp.in     = c(0.05, 0.025)
		pr.in     = c(0.001, 0.001)
	} else if(model=="WE"){
		mx.fA     = function(x, th) th[,1] * th[,2]^th[,1] * x^(th[,1]-1)
		Sx.fA     = function(x, th) exp(-(th[,2]*x)^th[,1])
		nthA      = 2
		lowA      = c(0, 0)
		th.in     = c(1, 0.1)
		jp.in     = c(0.01, 0.001)
		pr.in     = c(1, 0.01)
	} else if(model=="LO"){
		mx.fA     = function(x, th) exp(th[,1] + th[,2]*x)/(1+th[,3]*exp(th[,1])/th[,2]*(exp(th[,2]*x)-1))
		Sx.fA     = function(x, th) (1+th[,3]*exp(th[,1])/th[,2]*(exp(th[,2]*x)-1))^(-1/th[,3])
		nthA      = 3
		lowA      = c(-Inf, 0, 0)
		th.in     = c(-1, .25, 0.075)
		jp.in     = c(0.001, 0.001, 0.001)
		pr.in     = c(-2, 0.1, 0.001)
	}
	thnameA   = paste("b",1:nthA-1,sep="")

	# Extended model for different mortality shapes:
	if(model=="EX") Shape = "simple"
	if(Shape=="simple"){
		mx.fun    = function(x, Th, ga) mx.fA(x, Th) * exp(ga)
		Sx.fun    = function(x, Th, ga) Sx.fA(x, Th)^exp(ga)
		nTh       = nthA
		Low       = lowA
		Th.in     = th.in
		Jp.in     = jp.in
		Pr.in     = pr.in
		Thname    = thnameA
	} else if(Shape=="Makeham"){
		mx.fun    = function(x, Th, ga) (Th[,1] + mx.fA(x, matrix(Th[,-1], ncol=nthA))) * exp(ga)
		Sx.fun    = function(x, Th, ga) (exp(-Th[,1]*x) * Sx.fA(x, matrix(Th[,-1], ncol=nthA)))^exp(ga)
		nTh       = nthA + 1
		Th.in     = c(0, th.in)
		Jp.in     = c(0.01, jp.in)
		Pr.in     = c(0, pr.in)
		Low       = c(-Inf, lowA)
		if(model=="GO") Low = c(-Inf, -Inf, 0)
		Thname    = c("c", thnameA)
	} else if(Shape=="bathtub"){
		mx.fun    = function(x, Th, ga) (exp(Th[,1]-Th[,2]*x) + Th[,3] + mx.fA(x, matrix(Th[,-c(1:3)], ncol=nthA))) * exp(ga)
		Sx.fun    = function(x, Th, ga) (exp(exp(Th[,1])/Th[,2]*(exp(-Th[,2]*x)-1)-Th[,3]*x) * Sx.fA(x, matrix(Th[,-c(1:3)], ncol=nthA)))^exp(ga)
		nTh       = nthA + 3
		Th.in     = c(-0.1,0.5,0, th.in)
		Jp.in     = c(0.001, 0.001, 0.01, jp.in)
		Pr.in     = c(-2, 0.01, 0, pr.in)
		Low       = c(-Inf, 0, -Inf, lowA)
		if(model=="GO") Low = c(-Inf, 0, -Inf, -Inf, 0)
		Thname    = c("a0", "a1", "c", thnameA)
	}

	fx.fun    = function(x, Th, ga) mx.fun(x, Th, ga) * Sx.fun(x, Th, ga)

	S.x         = function(Th) Sx.fun(xv, matrix(Th, ncol=nTh), gaa)
	m.x         = function(Th) mx.fun(xv, matrix(Th, ncol=nTh), gaa)

	# 2.2 object processing:
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

	c.low <- function(th){
		
		if(Shape=="Makeham"){
			if(model=="GO"){
				cl = ifelse(th[3] > 0, -exp(th[2]), 0)
			} else if(model=="WE"){
				cl = 0
			} else if(model=="LO"){
				cl = ifelse(th[2]>th[3]*exp(th[1]), -exp(th[2]), 0)
			} 
		}
		if(Shape=="bathtub"){
			if(model=="GO"){
				x.minf = (th[1]+log(th[2]) - th[4]-log(th[5]))/(th[2] + th[5])
			} else if(model=="LO" | model=="WE"){
				xx     = seq(0,100,0.1)
				mxx    = mx.fun(xx, matrix(th,length(xx),nTh,byrow=TRUE),0)
				x.minf = which(mxx==min(mxx))[1]
			}
			cl     = -exp(th[1]-th[2]*(x.minf)) - mx.fA(x.minf,matrix(th[-c(1:3)],1,3))
		}
		return(cl)
	}


	CheckPars   = function(par, user.par, pnam){
		if(is.null(user.par)){
			pm        = matrix(par, nza, nTh, byrow=TRUE)
			dimnames(pm) = dimnames(low)
		} else {
			lp        = length(user.par)
			if(!is.element(lp, c(nTh, nTh*nza))){
				pm     = NULL
				if(!is.null(dim(user.par))){
					stop(paste("\nDimensions of ", pnam, " matrix for the mortality model parameters are incorrect.\n\nProvide a single vector of length ", nTh, " or a matrix of dimensions ",nza ," times ", nTh, ".(i.e. number of categorical covariates times number of parameters for model ", model," with ",Shape, " shape).", sep=""), call.=FALSE)
				} else if(is.null(dim(user.par))){
					stop(paste("\nLength of ", pnam, " vector for the mortality model parameters is incorrect.\n\nProvide a single vector of length ", nTh, " or a matrix of dimensions ",nza ," times ", nTh, ".(i.e. number of categorical covariates times number of parameters for model ", model," with ",Shape, " shape).", sep=""), call.=FALSE)
				}
			} else {
				if(!is.null(dim(user.par))){
					pm    = user.par
				} else {
					pm       = matrix(user.par, nza, nTh, byrow=TRUE)
				}
				dimnames(pm) = dimnames(low)
			}
		}
		return(pm)
	}

	paralvars   = c("mx.fA", "Sx.fA", "fx.fun", "mx.fun", "Sx.fun", "ObsMatFun", "c.low", "Thname", "nthA")
	
	# 2. Input error checking and data extraction:
	# 2.1 Data errors:
	tempcheck   = DataCheck(object, ststart, stend, silent=TRUE)
	if(tempcheck[[1]] == FALSE) stop("You have an error in Dataframe 'object',\nplease use function 'DataCheck'\n", call.=FALSE)

    # 2.2 Extract raw data:
	Ti          = ststart
	Tf          = stend
	st          = Ti:Tf
	nt          = length(st)
	idnames     = object[,1]
	n           = nrow(object)
	bd          = as.matrix(object[,2:3])
	Y           = as.matrix(object[,1:nt+3]); colnames(Y) = st

	paralvars   = c(paralvars, "Ti", "Tf", "st", "nt", "n", "bd", "Y", "rptp", "progr.plot") 

	# 2.3 Extract covariates:
	# a) Find if there are covariates:
	if(ncol(object)>nt+3){
		Z         = as.matrix(object[,(nt+4):ncol(object)])
		Ith       = Ith.fun(Z)
		Covars    = TRUE
		
		# Find if the model is Prop. Hazards:
		if(covar.str=="prop.haz"){
			Zc     = Z
			Za     = matrix(1, n, 1); colnames(Za) = "NZa"
			Cat    = FALSE
			Cont   = TRUE
		} else {
			
			# If all covariates should be included in the mortality section:
			if(covar.str=="all.in.mort" & !is.null(Ith$cont)){
				Za   = Z
				Zac  = matrix(Z[,Ith$cont], nrow(Z), length(Ith$cont))
				colnames(Zac) = names(Ith$cont)
				Za[,Ith$cont] = t(t(Zac)-apply(Zac,2,mean))
				Za   = Za[,c(Ith$int,Ith$cat, Ith$cont)]
				if(is.null(Ith$cat) & is.null(Ith$int)){
					Za   = cbind(1,Za)
					colnames(Za) = c("Intercept", names(Ith$cont))
				}
				Ith   = Ith.fun(Za)
				Zc   =  matrix(0,n,1); colnames(Zc) = "NZc"
				Cont   = FALSE
			} else {
		
				# Find if there are continuous covariates:
				if(!is.null(Ith$cont) & covar.str!="all.in.mort"){
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

	paralvars   = c(paralvars,"Za","Zc","Cont","nza","nzc", "Ith") 

    # 2.4 Check that niter, burnin, and thinning are compatible.
    if(burnin>niter) stop("\nObject 'burnin' larger than 'niter'.", call.=FALSE)
    if(thinning>niter) stop("\nObject 'thinning' larger than 'niter'.", call.=FALSE)
    
    # 1.4 Model type, shape and covariate structure:
    if(!is.element(model, c("EX","GO","WE","LO"))) stop("\nModel misspecification: specify available models (i.e. 'EX', 'GO', 'WE' or 'LO')\n", call.=FALSE)
    if(!is.element(Shape, c("simple","Makeham","bathtub"))) stop("\nShape misspecification: specify available shapes (i.e. 'simple','Makeham' or 'bathtub')\n", call.=FALSE)
    if(!is.element(covar.str, c("mixed","prop.haz","all.in.mort"))) stop("\nCovariate structure misspecification: specify available structures (i.e. 'mixed','prop.haz' or 'all.in.mort')\n", call.=FALSE)
    
    # 2.5 All covariates in mortality:
    if(covar.str == "all.in.mort"){
    	if(!is.null(Ith$cont)){
    	if(model!="GO") warning("To test effects of continuous and categorical covariates in rate parameter of mortality functon only simple Gompertz (GO) model can be used. Model and Shape arguments were changed to 'GO' and 'simple', respectively.\n", call.=FALSE)
    	model        = "GO"
    	Shape        = "simple"
    	} else {
    		warning("No continuous covariates were included in the data. Argument 'covar.str' will be set to 'mixed'.\n", call.=FALSE)
    		covar.str = 'mixed'
    	}
    }
    
	paralvars   = c(paralvars, "model", "Shape", "covar.str")
	
	# 3. MCMC prep:
	# 3.1 Model Matrix and lower limits for parameters:
	thname      = paste(rep(Thname,each=nza), "[",rep(colnames(Za), nTh),"]", sep="")
	if(nza==1) thname = Thname
	nthm        = length(thname)
	low         = matrix(Low,nza, nTh, byrow=TRUE)
	dimnames(low) = list(colnames(Za), Thname)
	ganame      = paste("gamma[",colnames(Zc),"]",sep=""); if(nzc == 1) ganame = "gamma"
	piname      = ifelse(length(rptp)==1, "pi", paste("pi[", rptp,"]",sep=""))
	poname      = c("post[th,ga]", "post[X0]", "post[full]")

	paralvars   = c(paralvars, "nTh","nthm", "thname", "ganame", "piname", "poname", "low") 

	# 3.2 MCMC setup variables:
	ng          = niter
	bng         = burnin
	thint       = thinning

	paralvars   = c(paralvars, "ng", "bng", "thint") 

	# 3.3 Verify th.jumps, initial parameters and th.priors: 
	# 3.3.1 Survival model:
	# a) Initial parameters:
	thg        = CheckPars(Th.in, th.ini.pars, "theta")
	if(covar.str=="all.in.mort" & is.null(th.ini.pars)) thg[names(Ith$cont),] = 0
	thgini    = thg

	# b) Jumps:
	thj       = CheckPars(Jp.in, th.jumps, "jumps")
	if(covar.str=="all.in.mort" & is.null(th.jumps)) thj[names(Ith$cont),] = 0.001
	thjini    = thj

	# c) Priors:
	thp       = CheckPars(Pr.in, th.priors, "priors")
	if(covar.str=="all.in.mort" & is.null(th.priors)) thp[names(Ith$cont),] = 0
	thpini    = thp
	
	
	# 3.3.2 Proportional hazards section:
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
				stop(paste("\nLength of jumps for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
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
				stop(paste("\nLength of priors for the prop. hazards parameters is not equal to number of covariates (n =", nzc,").", sep=""), call.=FALSE)
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


	# 3.4 Model variables:
	# 3.4.1 Extract times of birth and death:
	bi          = bd[,1]
	di          = bd[,2]

	# 3.4.2 Define study duration:
	Dx          = (st[2]-st[1])
	Tm          = matrix(st, n, nt, byrow=TRUE)

	# 3.4.3 Calculate first and last time observed:
	ytemp       = t(t(Y) * st)
	li          = c(apply(ytemp,1,max))
	ytemp[ytemp==0] = 10000
	fi          = c(apply(ytemp,1,min))
	fi[fi==10000] = 0
	rm("ytemp")

	# 3.4.4 Calculate number of times detected:
	oi          = Y %*% rep(1, nt)

	paralvars   = c(paralvars, "bi", "di", "Dx", "Tm", "li", "fi", "oi") 

	# 3.4.5 Priors:
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
	Ex          = sum(xx*fx.fun(xx,zza %*% thp, zzc)*dxx)
	v.x         = function(x) Sx.fun(x,Zthp, Zgap)/Ex

	# d) Detection probability:
	idpi        = findInterval(st, rptp); names(idpi) = st
	npi         = length(unique(idpi))
	rho1        = 0.1
	rho2        = 0.1

	paralvars   = c(paralvars, "Zthp", "thv", "Zgap", "gav", "Ex", "v.x", "idpi", "npi", "rho1", "rho2") 

	# 3.4.6 Starting values:
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


	# 4.  Multiple MCMC function:
	multiMCMC  = function(sim){
		if(parallel) for(ii in 1:(sim*2)){}
		nlow     = low
		if(nsim > 1){
			vpar        = thg * 0 + 0.25
			vpar[thj==0] = 0
			if(covar.str=="all.in.mort") vpar[Ith$cont,] = 0.05
			thn         = matrix(rtnorm(nthm, thg, vpar, lower=nlow), nza, nTh, dimnames=dimnames(thg))
			if(Shape!="simple"){
				nlow[,'c']  = apply(thn, 1, c.low)
				idcl        = which(thn[,'c'] < nlow[,'c'])
				if(length(idcl)>0){
					for(cc in idcl) thn[cc,'c']   = c(rtnorm(1, thg[cc,'c'], 0.5, lower=nlow[cc,'c']))
				}
			}
			if(Cont) gag = rnorm(nzc, gag, 0.5) 
			thg         = thn
			
		}
		
	
		# Output tables:
		thing       = seq(bng, ng, by=thint)
		thgibbs     = matrix(NA,ng,nthm)
		colnames(thgibbs) = thname
		gagibbs     = matrix(0, ng, nzc)
		colnames(gagibbs) = ganame
		pigibbs     = matrix(NA, ng, npi)
		colnames(pigibbs) = piname
		bgibbs      = matrix(NA,length(thing),n)
		dgibbs      = bgibbs
		postm       = matrix(NA, ng, 3)
		colnames(postm) = poname
		thgibbs[1,] = thg
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
		if(progr.plot){
			if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
			devtype(width=2, height=0.5); progrpl = dev.cur()
			par(mar=rep(0,4))
		}
		while(g <= ng & !naflag){
			if(g==1){cat("MCMC is running...\n")}
		
			# 1.- SAMPLING:
			# a) Sample survival parameters:
			thn         = matrix(rtnorm(nthm, thg, thj, lower=low), nza, nTh, dimnames=dimnames(thg))
			if(Shape!="simple"){
				nlow[,'c']  = apply(thn, 1, c.low)
				idcl        = which(thn[,'c'] < nlow[,'c'])
				if(length(idcl)>0){
					for(cc in idcl) thn[cc,'c']   = c(rtnorm(1, thg[cc,'c'], 0.5, lower=nlow[cc,'c']))
				}
			}
			if(Cont) gan = rnorm(nzc, gag, gaj) else gan = gag
			

			Zthn        = Za %*% thn
			Zgan        = Zc %*% gan
			idtrg       = which(bg<Ti)
			lidt        = length(idtrg)

			p.thg       = log(fx.fun(xg + 0.5*Dx, Zthg, Zgag))
			p.thg[idtrg] = p.thg[idtrg] - log(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, matrix(Zthg[idtrg,], ncol= nTh), matrix(Zgag[idtrg,], ncol= nzc)))
			p.thg[p.thg==-Inf] = -1e300
			p.thg = sum(p.thg) + sum(dtnorm(c(thg), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gag, gap, gav, log=TRUE))

			p.thn       = log(fx.fun(xg + 0.5*Dx, Zthn, Zgan))
			p.thn[idtrg] = p.thn[idtrg] - log(Sx.fun(Ti-bg[idtrg] + 0.5*Dx, matrix(Zthn[idtrg,], ncol=nTh), matrix(Zgan[idtrg,], ncol=nzc)))
			p.thn[p.thn==-Inf] = -1e300
			p.thn = sum(p.thn) + sum(dtnorm(c(thn), c(thp), thv, lower=c(low), log=TRUE)) + sum(dnorm(gan, gap, gav, log=TRUE))

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
			bn[bi0][oi[bi0]>0]  = apply(cbind(bn[bi0][oi[bi0]>0],fi[bi0][oi[bi0]>0]-1),1,min)
			bn[bi0][oi[bi0]==0] = apply(cbind(bn[bi0][oi[bi0]==0],dg[bi0][oi[bi0]==0]-1),1,min)

			dn          = dg 
			dn[di0]     = dg[di0] + sample(-1:1, length(di0), replace=TRUE) 
			dn[di0]     = apply(cbind(dn[di0],bn[di0],li[di0]+1),1,max) 

			xn          = dn - bn

			Fn          = c(apply(cbind(Ti, bn+1), 1, max))
			Ln          = c(apply(cbind(Tf, dn-1), 1, min))
			On          = ObsMatFun(Fn, Ln, Tm)
    
			p.bdg       = log(fx.fun(xg + 0.5*Dx, Zthg, Zgag))
			p.bdg[p.bdg==-Inf] = -1e300
			p.bdg       = p.bdg + (Og - lfi) %*% log(1-Pig) + log(v.x(xg + 0.5*Dx))

			p.bdn       = log(fx.fun(xn + 0.5*Dx, Zthg, Zgag))
			p.bdn[p.bdn==-Inf] = -1e300
			p.bdn       = p.bdn + (On - lfi) %*% log(1-Pig) + log(v.x(xn + 0.5*Dx))
	
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
			thgibbs[g,] = thg
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


	# 5. Run (multi) MCMC:
	if(nsim==1){
		parallel = FALSE
	}
	if(nsim > 1) cat("Multiple simulations started...\n\n") else cat("Simulation started...\n\n")
	Start      = Sys.time()
	if(parallel){
		availpkg     = available.packages()
		if(!is.element("snowfall", availpkg)){
			warning("\nPackage 'snowfall' is not installed.\nSimulations will not be ran in parallel (computing time will be longer...)\n")
			out      = lapply(1:nsim, multiMCMC)
		} else {
			require(snowfall)
			sfInit(parallel=TRUE, cpus=ncpus);
			sfExport(list=c(paralvars, "parallel", "nsim"))
			sfLibrary(msm)
			out     = sfClusterApplyLB(1:nsim, multiMCMC)
			sfStop()
		}
	} else {
		out      = lapply(1:nsim, multiMCMC)
	}
	End        = Sys.time()


	# Report if all simulations ran through:
	simname    = paste("Sim.", (1:nsim), sep="")
	names(object) = simname
	full.runs  = rep(0,nsim); names(full.runs) = simname
	last.steps = full.runs
	for(i in 1:nsim){
		last.steps[i] = out[[i]]$g		
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
			cat(paste("MCMC stopped at step ", out[[1]]$g,"\nPdf of ages at death equal to 0 for some individuals.\nModify starting parameters or jumps\n", sep=""))
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

	# 6. Diagnostics:
	thing      = seq(burnin, niter, thinning)
	nthin      = length(thing)

	# 6.1 Thinned result matrices:
	if(Cont) pname  = c(thname, ganame, piname, poname) else pname = c(thname, piname, poname)
	Pmat       = matrix(NA, ng*nsim, length(pname))
	dimnames(Pmat) = list(rep(simname, each=ng), pname)
	Bimat      = matrix(NA, nthin*nsim, n)
	rownames(Bimat) = rep(simname, each=nthin)
	Dimat      = Bimat
	idthin     = rep(0, ng*nsim)

	for(i in 1:nsim){
		Idsim       = which(rownames(Pmat)==simname[i])
		if(Cont){
			Pmat[Idsim,] = cbind(out[[i]]$theta, out[[i]]$gamma, out[[i]]$pi, out[[i]]$post)
		} else {
			Pmat[Idsim,] = cbind(out[[i]]$theta, out[[i]]$pi, out[[i]]$post)
		}
		idthin[Idsim[thing]] = 1
		Idsim       = which(rownames(Bimat)==simname[i])
		Bimat[Idsim,] = out[[i]]$bi
		Dimat[Idsim,] = out[[i]]$di
	}
		
	# 7.2 Basic summary statistics for parameters:
	pmat      = Pmat[idthin==1,-(c(ncol(Pmat)-c(2:0)))]
	coef      = cbind(apply(pmat, 2, mean, na.rm=TRUE), apply(pmat, 2, sd, na.rm=TRUE), t(apply(pmat, 2, quantile, c(0.025, 0.975), na.rm=TRUE)), NA, NA, NA)
	colnames(coef) = c("Estimate", "StdErr", "Lower95%CI", "Upper95%CI", "SerAutocor", "UpdateRate", "PotScaleReduc")
	if(length(id.failed) < nsim){
		idfix      = which(thj==0)
		if(length(idfix)>0){
			coef[-idfix,"SerAutocor"]  = apply(pmat[,-c(idfix)],2, function(x) cor(x[-1],x[-length(x)], use="complete.obs"))
			coef[idfix,"SerAutocor"]  = 1
		} else {
			coef[,"SerAutocor"]  = apply(pmat,2, function(x) cor(x[-1],x[-length(x)], use="complete.obs"))
		}
		coef[,"UpdateRate"]  = apply(Pmat[,-c(ncol(Pmat)-c(2:0))], 2, function(x) length(which(diff(x[!is.na(x)])!=0))/length(x[!is.na(x)]))
	} 
	
	Pmat      = cbind(idthin, Pmat)

	# 6.3 Convergence and model selection:
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
			Rhat[Varpl==0] = 1
			conv        = cbind(B,W,Varpl,Rhat)
			rownames(conv) = colnames(pmat)
			coef[,ncol(coef)] = conv[,'Rhat']

			# Report if convergence was reached:
			idnconv     = which(conv[,'Rhat']< 0.95 | conv[,'Rhat']>1.1)
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
				k          = npi + nthm
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

		# 6.3.3 Summary times of birth and ages at death:
		xq        = apply(Dimat - Bimat, 2, quantile, c(0.5, 0.025, 0.975))
		bq        = apply(Bimat, 2, quantile, c(0.5, 0.025, 0.975))

		# 6.3.4 Summary Survival and mortality:
		thmat      = pmat[,thname]
		if(Cont){
			rzc       = apply(Zc, 2, quantile, c(0.5, 0.025, 0.975))
			rownames(rzc)    = c("Med.","Lower","Upper")
			gave      = apply(as.matrix(pmat[,which(substr(colnames(pmat),1,2)=="ga")]),2,mean)
		} else if(covar.str=="all.in.mort"){
			rzc       = apply(as.matrix(Za[,Ith$cont]), 2, quantile, c(0.5, 0.025, 0.975))
			dimnames(rzc)    = list(c("Med.","Lower","Upper"), names(Ith$cont))
			gave      = 0
			zcname    = colnames(rzc)
		} else {
			rzc       = matrix(0,1,1,dimnames= list("nc","nc"))
			gave      = 0
			zcname    = c("")
		}
		Sxq       = list()
		mxq       = list()
		xvec      = list()
		zaname    = c(names(Ith$int), names(Ith$cat))
		for(i in 1:length(zaname)){
			idza    = which(Za[,i]==1)
			xv      = seq(0,ceiling(max(xq[1,idza])*1.1),0.1)
			xvec[[zaname[i]]] = xv
			for(j in 1:ncol(rzc)){
				Sxq[[zaname[i]]][[colnames(rzc)[j]]] = array(0, dim=c(length(xv), 3, nrow(rzc)), dimnames=list(NULL, c("50%", "2.5%", "97.5%"), rownames(rzc)))
				
				mxq[[zaname[i]]][[colnames(rzc)[j]]] = Sxq[[zaname[i]]][[colnames(rzc)[j]]]
				for(k in 1:nrow(rzc)){
					gaa    = sum(gave * rzc[k,j])
					Cols   = paste(Thname, "[",zaname[i],"]", sep="")
					if(nza==1) Cols = Thname
					Thm    = thmat[,Cols]
					if(covar.str=="all.in.mort"){
						Thm    = Thm + thmat[,paste(Thname, "[",names(Ith$cont)[j],"]", sep="")]* rzc[k,j]
					}
					Sxq[[zaname[i]]][[colnames(rzc)[j]]][,,k] = t(apply(apply(Thm,1,S.x),1, quantile, c(0.5,0.025,0.975)))
				
					mxq[[zaname[i]]][[colnames(rzc)[j]]][,,k] = t(apply(apply(Thm,1,m.x),1, quantile, c(0.5,0.025,0.975)))
				}
			}
		}
		
		# 6.3.5 Calculate life table from estimated ages at death:
		if(lifetable){
			LT     = list()
			for(i in zaname){
				idza    = which(Za[,i]==1 & bq[1,]>=ststart)
				x       = xq[1,idza]
				LT[[i]] = CohortLT(x, ax=0.5, n=1)
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
	ModelSpecs         = c(model, Shape, covar.str, paste(names(Ith$cat), collapse = ", "), paste(names(Ith$cont), collapse=", "))
	names(ModelSpecs)  = c("model","Shape", "Covar. structure", "Categorical", "Continuous")
	JumpPriors         = cbind(c(thj,gaj), c(thp,gap))
	dimnames(JumpPriors) = list(c(thname,ganame), c("Jump.sd", "Mean.priors"))
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
