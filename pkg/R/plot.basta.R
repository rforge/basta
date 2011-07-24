plot.basta <-
function(x, plot.type="traces", tracename = "theta", ...){
   	Palette    = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")

	if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
	
	if(plot.type=="traces"){
	plotname     = c("theta","gamma","pi", "post")

	if(!is.element(tracename, plotname)) stop(paste("Wrong 'tracename' argument. Valid arguments are:", paste(paste("'",plotname,"'", sep=""), collapse=", "),".\n"), call.=FALSE)
	
	pnames      = substr(colnames(x$Par)[-1],1,2)
	npars       = table(pnames)[c("th","ga","pi","po")]
	names(npars) = c("th","ga","pi","po")
	npars[is.na(npars)] = 0
	if(tracename=="gamma" & npars["ga"]==0) stop("\nTrace plots cannot be drawn for 'gamma' parameters.\nNo proportional hazards arguments were evaluated.", call.=FALSE)
	

	# PLOTS:
	# Trace plots for parameters:
	ng          = x$settings['niter']
	nsim        = x$settings['nsim' ]
	simname     = unique(rownames(x$Par))
	idpl        = which(pnames==substr(tracename,1,2))
	X           = x$Par[,-1][,idpl]
	p           = which(plotname == tracename)
	Cols        = Palette[round(seq(1,12, length=nsim))]
	model       = as.character(x$ModelSpecs['model'])
	if(model == "GO") nthm = 2 else if(model=="GM") nthm = 3 else nthm = 5
	ydim        = c(nthm, ceiling(npars['ga']/2), ceiling(npars['pi']/2), 2)
	xdim        = c(npars['th']/nthm, 2, 2, 2)
	if(ydim[3]==1) xdim[3] = 1

	op          = par(mfrow=c(ydim[p], xdim[p]), mar=c(3,3,3,1))
	for(i in 1:npars[p]){
		if(npars[p] > 1)x = X[,i] else x = X
		yl          = range(x, na.rm=TRUE)
			plot(c(1,ng), yl, col=NA, xlab="Iteration", ylab="", main=colnames(X)[i], frame.plot=FALSE)
			for(j in 1:nsim) lines(x[which(names(x) == simname[j])], type='l', col=Cols[j], lwd=1.5)
	}
	par(op)

	} else {
		
		if(is.null(x$Sx)){
			stop("MCMC runs on BaSTA x did not finish.\n Survival and mortality plots cannot be constructed, verify model input and run again\n", call.=FALSE)
		}

		zname      = names(x$Sx)
		nza        = length(zname)
		Bord       = Palette[round(seq(1,12, length=nza))]
		Cols       = adjustcolor(Bord, alpha.f=0.5)
	
		op         = par(mfrow=c(2,1), mar=c(4,4,3,2))

		# Plot survival probability:
		mxv          = ceiling(max(unlist(x$xv))/5)*5
		plot(c(0,mxv), range(0,1), col=NA, xlab="", ylab=expression(S(x)), main="Survival probability", frame.plot=FALSE, ...)
		for(i in 1:nza){
			xv       = x$xv[[i]]
			polygon(c(xv, rev(xv)), c(x$Sx[[i]][[1]][2,], rev(x$Sx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
			lines(xv, x$Sx[[i]][[1]][1,], col=Bord[i], lty=3)
		}
		legend('topright', zname, pch=15, pt.cex=3, cex=1.5, col=Cols, bty='n')

		# Plot mortality rates:
		ylmx       = c(0, round(max(unlist(x$mx))))
		plot(c(0,mxv), ylmx, col=NA, xlab="Age (x)", ylab=expression(mu(x)), main="Mortality rate", frame.plot=FALSE, ylim=ylmx, ...)
		for(i in 1:nza){
			xv       = x$xv[[i]]
			polygon(c(xv, rev(xv)), c(x$mx[[i]][[1]][2,], rev(x$mx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
			lines(xv, x$mx[[i]][[1]][1,], col=Bord[i], lty=3)
		} 
		par(op)
	}
}

