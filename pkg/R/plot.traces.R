plot.traces <-
function(outBS, tracename = "theta"){
	require(RColorBrewer)
	if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
	plotname     = c("theta","gamma","pi", "post")
	if(!is.element(tracename, plotname)) stop(paste("Wrong 'tracename' argument. Valid arguments are:", paste(paste("'",plotname,"'", sep=""), collapse=", "),".\n"), call.=FALSE)
	
	if(tracename=="gamma" & is.na(max(outBS$results$gamma))) stop("\nTrace plots cannot be drawn for 'gamma' parameters.\nNo proportional hazards arguments were evaluated.", call.=FALSE)

	# PLOTS:
	# Trace plots for parameters:
	p           = which(plotname == tracename)
	nza         = ncol(outBS$data$Za)
	nzc         = ncol(outBS$data$Zc)
	ydim        = c(sum(outBS$input$modm[outBS$input$idm,]), ceiling(nzc/2), ceiling((ncol(outBS$results$pi))/2), 2)
	xdim        = c(nza, 2, 2, 2)

	x           = outBS$results[[tracename]]
	Main        = list(theta = colnames(outBS$results$theta), gamma=colnames(outBS$results$gamma), pi = colnames(outBS$results$pi), post = expression(paste(p,"(",theta," | ",X,")"), paste(p,"(",X[0]," | ",X[1],",",theta,",",pi,",",")"), paste(p,"(",X[0],",",theta,",",pi,"| ... ",")")))
	if(ncol(x)==1 & tracename=="pi"){
		Main$pi = "pi"
		ydim[2] = 1
		xdim[2] = 1
	} 

	par(mfrow=c(ydim[p], xdim[p]), mar=c(3,3,2,1))
	for(i in 1:ncol(x)){
		
		if(length(dim(x))<3){
			plot(x[,i], type='l', xlab="Iteration", ylab="", main=Main[[p]][i], frame.plot=FALSE)
		} else {
			yl    = range(x[,i,], na.rm=TRUE)
			plot(c(1,nrow(x)), yl, col=NA, xlab="Iteration", ylab="", main=Main[[p]][i], frame.plot=FALSE)
			for(j in 1:dim(x)[3]) lines(x[,i,j], type='l', col=brewer.pal(12, "Paired")[j])
		}
	}
}

