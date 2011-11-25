PlotMultiMort <- function(modList){

ylmx.temp = NULL
for(m in 1:length(modList)){
    x = get(modList[m])
    ylmx.temp = append(ylmx.temp,round(max(unlist(x$mx))))
}
    ylmx            <- c(0, round(max(ylmx.temp)))


for(m in 1:length(modList)){
    x = get(modList[m])
    mxv <- ceiling(max(unlist(x$xv)) / 5) * 5
    if(max(ylmx)==Inf) {ylmx <- c(0, 25)}
    length.cat        <- ncol(x$Zcat)


if(m==1){    plot(x           = c(0,mxv), 
         y           = ylmx, 
         col         = NA, 
         ylim        = ylmx,
         xlab        = "Age (x)", 
         ylab        = expression(mu(x)), 
         main        = "Mortality rate")}

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

      if(length.cat>1){
      xvEmp =  x$xv[[i]][which(x$xv[[i]]<=max.age)]
      mxEmp =  x$mx[[i]][[1]][,,1]
      mxEmp =  mxEmp[which(x$xv[[i]]<=max.age),]
      }else{
      xvEmp =  x$xv[[1]][which(x$xv[[1]]<=max.age)]
      mxEmp = x$mx[[1]][[1]][,,1]
      mxEmp = mxEmp[which(x$xv[[i]]<=max.age),]}
      
            polygon(x      = c(xvEmp, rev(xvEmp)), 
              y = c(mxEmp[,2], rev(mxEmp[,3])),
              col    = paste(substr(Cols[i],1,7),"99",sep=""), 
              border = Bord[i])}
}
}
}