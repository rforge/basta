plot.traces <-
function(outbasta, tracename = "theta"){
   	Palette    = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")

	if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows

	plotname     = c("theta","gamma","pi", "post")

	if(!is.element(tracename, plotname)) stop(paste("Wrong 'tracename' argument. Valid arguments are:", paste(paste("'",plotname,"'", sep=""), collapse=", "),".\n"), call.=FALSE)
	
	pnames      = substr(colnames(outbasta$Par)[-1],1,2)
	npars       = table(pnames)[c("th","ga","pi","po")]
	names(npars) = c("th","ga","pi","po")
	npars[is.na(npars)] = 0
	if(tracename=="gamma" & npars["ga"]==0) stop("\nTrace plots cannot be drawn for 'gamma' parameters.\nNo proportional hazards arguments were evaluated.", call.=FALSE)
	

	# PLOTS:
	# Trace plots for parameters:
	ng          = outbasta$Settings['niter']
	nsim        = outbasta$Settings['nsim' ]
	simname     = unique(rownames(outbasta$Par))
	idpl        = which(pnames==substr(tracename,1,2))
	X           = outbasta$Par[,-1][,idpl]
	p           = which(plotname == tracename)
	Cols        = Palette[round(seq(1,12, length=nsim))]
	model       = as.character(outbasta$ModelSpecs['model'])
	if(model == "GO") nthm = 2 else if(model=="GM") nthm = 3 else nthm = 5
	ydim        = c(nthm, ceiling(npars['ga']/2), ceiling(npars['pi']/2), 2)
	xdim        = c(npars['th']/nthm, 2, 2, 2)
	if(ydim[3]==1) xdim[3] = 1

	par(mfrow=c(ydim[p], xdim[p]), mar=c(3,3,2,1))
	for(i in 1:npars[p]){
		x           = X[,i]
		yl          = range(x, na.rm=TRUE)
			plot(c(1,ng), yl, col=NA, xlab="Iteration", ylab="", main=colnames(X)[i], frame.plot=FALSE)
			for(j in 1:nsim) lines(x[which(names(x) == simname[j])], type='l', col=Cols[j], lwd=1.5)
	}
}

