PlotLexis <- function(x,ci=1,ji=0.5,col=c("#007FFF15","#FF000015"),xlab="Year",ylab="Age (yrs)"){

BirthTime  = x$Bq[ci,]
AgeAtDeath = x$Xq[ci,]
DeathTime  = BirthTime+AgeAtDeath
nCat       = ncol(x$Zcat)

par(mfrow=c(nCat,1))
for(nc in 1:nCat){
    plot(c(min(BirthTime),max(BirthTime)),c(0,max(AgeAtDeath)),type="n",axes=T,xlab=xlab,ylab=ylab,xlim=c(min(BirthTime),max(DeathTime)))

    ssbt  = BirthTime[x$Zcat[,nc]==1]
    ssdt  = DeathTime[x$Zcat[,nc]==1]
    ssaad = ssdt-ssbt

    for (i in 1:length(ssbt)){
        jitterval = runif(1,0,1)*ji
        lines(c(ssbt[i]+jitterval,ssdt[i]+jitterval),(c(0,ssaad[i])),col=col[nc],lwd=4)
        }
    }
}