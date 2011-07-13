FancyPlot <-
function(outBS, open.new = FALSE){

   	Palette    = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")

	if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows

	nth        = 5
	zname      = names(outBS$q$Sx)
	nza        = length(zname)
	idth       = which(outBS$inp$modm[outBS$inp$idm,]==1)
	nthn       = length(idth)
	pname      = paste(rep(colnames(outBS$input$modm)[idth],each=nza), "[",rep(zname, nthn),"]", sep="")
	parname    = colnames(outBS$input$modm)[idth]
	expar     = expression(alpha[1],beta[1],c,alpha[2],beta[2])[idth]
	Bord       = Palette[round(seq(1,12, length=nza))]
	Cols       = adjustcolor(Bord, alpha.f=0.5)


	# Build plots:	
	if(open.new) devtype(width=6, height=6)
	layout(matrix(c(rep(1:nthn, each=2), rep(rep(c(nthn+1, nthn+2),each=nthn), 2)), nthn*2, 3), widths = rep(2, 3), heights=rep(1, nthn))
	par(mar=c(3,3,0.5,0.5))
	for(i in 1:nthn){
		xz     = list()
		dez    = list()
		ylz    = rep(NA, nza)
		xlz    = matrix(0,nza,2)
		for(j in 1:nza){
			xz[[zname[j]]]  = outBS$inf$t$mat[,paste(parname[i],"[",zname[j],"]",sep="")]
			dez[[zname[j]]] = density(xz[[zname[j]]])
			ylz[j]          = max(dez[[zname[j]]]$y)
			xlz[j,]         = range(dez[[j]]$x)
		}
		xr     = range(xlz)
		xl     = c(floor(xr[1]*10)/10, ceiling(xr[2]*10)/10)
		xd     = ceiling(diff(xl)*10)/10
		plot(dez[[1]], xlab="", ylab="", xlim=xl, ylim=c(0, max(ylz)), lwd=3, axes=FALSE, main="", col=NA)
		for(j in 1:nza) polygon(c(dez[[j]]$x, dez[[j]]$x[1]), c(dez[[j]]$y, dez[[j]]$y[1]), col=Cols[j], border=Bord[j], lwd=1.5)
		axis(1, at=seq(xl[1], xl[2], length=5), line=0.5, labels=NA, tcl=0.4)
		axis(1, at=seq(xl[1], xl[2], length=3), lwd=NA)
		mtext(expar[i], 2, line=0, at=max(ylz)*0.8, las=2, cex=1.25)
	}

	par(mar=c(4, 7, 0.5, 0.5))
	# Plot survival probability:
	mxv          = ceiling(max(unlist(outBS$q$x))/5)*5
	plot(c(0,mxv), range(0,1), col=NA, axes=FALSE, xlab="", ylab="")
	for(i in 1:nza){
		xv       = outBS$q$x[[i]]
		polygon(c(xv, rev(xv)), c(outBS$q$Sx[[i]][[1]][2,], rev(outBS$q$Sx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
		lines(xv, outBS$q$Sx[[i]][[1]][1,], col=Bord[i], lty=3)
	}
	legend('topright', zname, pch=15, pt.cex=3, cex=1.5, col=Cols, bty='n')

	axis(1, seq(0,mxv, 5), labels=NA, tcl=0.4, line=0.5)
	axis(2, seq(0, 1, 0.2), tcl=0.4, las=2, cex.axis=1.2)
	mtext(expression(paste("Survival prob. ", italic(S(x)))), 2, line=3.5, cex=1.25)

	# Plot mortality rates:
	ylmx       = c(0, round(max(unlist(outBS$q$mx))))
	plot(c(0,mxv), ylmx, col=NA, axes=FALSE, xlab="", ylab="")
	for(i in 1:nza){
		xv       = outBS$q$x[[i]]
		polygon(c(xv, rev(xv)), c(outBS$q$mx[[i]][[1]][2,], rev(outBS$q$mx[[i]][[1]][3,])), col=Cols[i], border=Bord[i])
		lines(xv, outBS$q$mx[[i]][[1]][1,], col=Bord[i], lty=3)
	} 

	axis(1, seq(0,mxv, 5), labels=NA, tcl=0.4, line=0.5)
	axis(1, at=seq(0,mxv, 5), lwd=NA, cex.axis=1.2)
	axis(2, seq(0, ylmx[2], length=5), tcl=0.4, las=2, cex.axis=1.2)
	mtext(expression(paste("Mortality rate ", italic(mu(x)))), 2, line=3.5, cex=1.25)
	mtext(expression(paste("Age ", italic(x), " (years)")), 1, cex=1.25, line=3)

}

