plot.SM <-
function(outBS, ...){
	
	if(is.null(outBS$q$Sx)){
		stop("MCMC runs on BaSTA object did not finish.\n Survival and mortality plots cannot be constructed, verify model input and run again\n", call.=FALSE)
	}

	Palette    = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
	
	zname      = names(outBS$q$Sx)
	nza        = length(zname)
	Bord       = Palette[round(seq(1,12, length=nza))]
	Cols       = adjustcolor(Bord, alpha.f=0.5)
	
	par(mfrow=c(2,1), mar=c(4,4,3,2))
	# Plot survival probability:
	mxv          = ceiling(max(unlist(outBS$q$x))/5)*5
	plot(c(0,mxv), range(0,1), col=NA, xlab="", ylab=expression(S(x)), main="Survival probability", frame.plot=FALSE, ...)
	for(i in 1:nza){
		xv       = outBS$q$x[[i]]
		polygon(c(xv, rev(xv)), c(outBS$q$Sx[[i]][[1]][2,], rev(outBS$q$Sx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
		lines(xv, outBS$q$Sx[[i]][[1]][1,], col=Bord[i], lty=3)
	}
	legend('topright', zname, pch=15, pt.cex=3, cex=1.5, col=Cols, bty='n')

	# Plot mortality rates:
	ylmx       = c(0, round(max(unlist(outBS$q$mx))))
	plot(c(0,mxv), ylmx, col=NA, xlab="Age (x)", ylab=expression(mu(x)), main="Mortality rate", frame.plot=FALSE, ylim=ylmx, ...)
	for(i in 1:nza){
		xv       = outBS$q$x[[i]]
		polygon(c(xv, rev(xv)), c(outBS$q$mx[[i]][[1]][2,], rev(outBS$q$mx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
		lines(xv, outBS$q$mx[[i]][[1]][1,], col=Bord[i], lty=3)
	} 
}

