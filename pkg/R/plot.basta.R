plot.basta <-
function(x, plot.trace = TRUE, trace.name = "theta", ...){

  Palette           <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
                         "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
                         "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  length.cat        <- ncol(x$Zcat)
  if(plot.trace){
  	# 1. Trace plots for parameters:
    traces          <- c("theta","gamma","pi", "post")
    if(!is.element(trace.name, traces)) {
      stop(paste("Wrong 'trace.name' argument. Valid arguments are:", 
      paste(paste("'", traces, "'", sep = ""), collapse = ", "), ".\n"), 
      call. = FALSE)
    }
    varNames        <- substr(colnames(x$Par)[-1], 1, 2)
    id.th           <- is.element(substr(varNames, 1, 1), c("a", "b", "c"))
    varNames[id.th] <- "th"
    nTraces         <- table(varNames)[c("th", "ga", "pi", "po")]
    names(nTraces)  <- c("th", "ga", "pi", "po")
    nTraces[is.na(nTraces)] <- 0
    if (trace.name == "gamma" & nTraces["ga"] == 0) {
      stop("\nTrace plots cannot be drawn for 'gamma' parameters.",
           "\nNo proportional hazards arguments were evaluated.", 
           call. = FALSE)
    }
    niter           <- x$settings['niter']
    nsim            <- x$settings['nsim' ]
    simNames        <- unique(rownames(x$Par))
    idpl            <- which(varNames == substr(trace.name, 1, 2))
    X               <- as.matrix(x$Par[, -1][, idpl])
    colnames(X)     <- colnames(x$Par)[-1][idpl]
    if (niter > 1e3) {
      xThin         <- round(seq(1, niter, length = 1e3))
    } else {
      xThin         <- 1:niter
    }
    p               <- which(traces == trace.name)
    Cols            <- Palette[round(seq(1, 12, length = nsim))]
    model           <- as.character(x$ModelSpecs['model'])
    shape           <- as.character(x$ModelSpecs['shape'])
    if (model == "EX") {
      nthm          <- 1
    } else if (model == "GO" | model == "WE") {
      nthm          <- 2
    }  else {
      nthm          <- 3
    }
    if (shape == "Makeham") {
      nthm          <- nthm + 1
    } else if(shape == "bathtub") {
      nthm          <- nthm + 3
    }
    ydim            <- c(nthm, ceiling(nTraces['ga'] / 2), 
                         ceiling(nTraces['pi'] / 2), 2)
    xdim            <- c(length.cat, 2, 2, 2)
    for(ii in 2:3) if (ydim[ii] == 1) xdim[ii] <- 1
    op              <- par(mfrow = c(ydim[p], xdim[p]), 
                           mar   = c(3, 3, 3, 1))
    for(i in 1:nTraces[p]){
      x             <- X[, i]
      yl            <- range(x, na.rm = TRUE)
      plot(x          = c(1, niter), 
           y          = yl, 
           col        = NA, 
           xlab       = "Iteration", 
           ylab       = "", 
           main       = colnames(X)[i], 
           frame.plot = FALSE)
      for(j in 1:nsim) {
        lines(x       = xThin, 
              y       = x[which(names(x) == simNames[j])][xThin],
              type    = 'l', 
              col     = Cols[j], 
              lwd     = 1.5)
      }
    }
    par(op)

  # 2. Plot survival and mortality:
  } else {

    if(is.null(x$Sx)){
      stop("MCMC runs on BaSTA did not finish.\n Survival and mortality ",
           "plots cannot be constructed, verify model input and run again.\n",
           call. = FALSE)
    }

    zname           <- names(x$Sx)
    length.cat      <- length(zname)
    Bord            <- Palette[round(seq(1, 12, length = length.cat))]
    Cols            <- adjustcolor(Bord, alpha.f = 0.5)
    op              <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

    # Plot survival probability:
    mxv             <- ceiling(max(unlist(x$xv)) / 5) * 5
    plot(x           = c(0, mxv), 
         y           = range(0, 1), 
         col         = NA, 
         xlab        = "", 
         ylab        = expression(S(x)), 
         main        = "Survival probability", 
         frame.plot  = FALSE, ...)
    for(i in 1:length.cat){
      xv            <- x$xv[[i]]
      polygon(x      = c(xv, rev(xv)), 
              y      = c(x$Sx[[i]][[1]][, 2, 1], rev(x$Sx[[i]][[1]][, 3, 1])), 
              col    = Cols[i], 
              border = Bord[i])

             lines(x        = xv, 
            y        = x$Sx[[i]][[1]][,1,1], 
            col      = Bord[i], 
            lty      = 3)

#Estimated ages data
max.age <- max(x$Xq[1,][x$Zcat[,i] == 1])

if(max.age>0){
      xvEmp = x$xv[[i]][which(x$xv[[i]]<=max.age)]
      if(length.cat>1){
      SxEmp =  x$Sx[[i]][[1]][,,1]
      SxEmp =  SxEmp[which(x$xv[[i]]<=max.age),]
      }else{SxEmp = x$Sx[[1]][[1]][,,1]
      SxEmp = SxEmp[which(x$xv[[i]]<=max.age),]}
      
            polygon(x      = c(xvEmp, rev(xvEmp)), 
              y = c(SxEmp[,2], rev(SxEmp[,3])),
              col    = paste(substr(Cols[i],1,7),"99",sep=""), 
              border = Bord[i])
              }
 
 }
    if (length.cat > 1) {
      legend(x       = 'topright', 
             legend  = zname, 
             pch     = 15, 
             pt.cex  = 3, 
             cex     = 1.25, 
             col     = Cols, 
             bty     = 'n')
    }

    # Plot mortality rates:
    ylmx            <- c(0, round(max(unlist(x$mx))))
    if(max(ylmx)==Inf) {ylmx <- c(0, 25)}
    if(max(ylmx)==0) {ylmx <- c(0, max(unlist(x$mx)))}

    plot(x           = c(0,mxv), 
         y           = ylmx, 
         col         = NA, 
         ylim        = ylmx,
         xlab        = "Age (x)", 
         ylab        = expression(mu(x)), 
         main        = "Mortality rate", 
         frame.plot  = FALSE, ...)
    for(i in 1:length.cat){
      xv            <- x$xv[[i]]
      polygon(x      = c(xv, rev(xv)), 
              y      = c(x$mx[[i]][[1]][,2,1], rev(x$mx[[i]][[1]][,3,1])), 
              col    = Cols[i], 
              border = Bord[i])
      lines(x        = xv, 
            y        = x$mx[[i]][[1]][, 1, 1], 
            col      = Bord[i], 
            lty      = 3)
            
#Empirical data
max.age <- max(x$Xq[1,][x$Zcat[,i] == 1])

if(max.age>0){

      xvEmp = x$xv[[i]][which(x$xv[[i]]<=max.age)]
      if(length.cat>1){
      mxEmp =  x$mx[[i]][[1]][,,1]
      mxEmp =  mxEmp[which(x$xv[[i]]<=max.age),]
      }else{mxEmp = x$mx[[1]][[1]][,,1]
      mxEmp = mxEmp[which(x$xv[[i]]<=max.age),]}
      
            polygon(x      = c(xvEmp, rev(xvEmp)), 
              y = c(mxEmp[,2], rev(mxEmp[,3])),
              col    = paste(substr(Cols[i],1,7),"99",sep=""), 
              border = Bord[i])}

    }
    par(op)
  }
}
