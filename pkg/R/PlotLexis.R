PlotLexis <- function(x, ci = 1, ji = 0.5, ...){

BirthTime  = x$Bq[ci,]
AgeAtDeath = x$Xq[ci,]
DeathTime  = BirthTime+AgeAtDeath
nCat       = ncol(x$Zcat)
CatNames   = colnames(x$Zcat)

par(mfrow=c(nCat,1))
for(nc in 1:nCat){
    plot(c(min(BirthTime),max(BirthTime)),c(0,max(AgeAtDeath)),type="n",xlim=c(min(BirthTime),max(DeathTime)),...)
    title(main=CatNames[nc])
    ssbt  = BirthTime[x$Zcat[,nc]==1]
    ssdt  = DeathTime[x$Zcat[,nc]==1]
    ssaad = ssdt-ssbt
    
    #Grid
    #abline(v=min(BirthTime):max(DeathTime),col="grey35")
    #abline(h=1:max(ceiling(AgeAtDeath)),col="grey35")

    for (i in 1:length(ssbt)){
        jitterval = runif(1,0,1)*ji
        lines(c(ssbt[i]+jitterval,ssdt[i]+jitterval),(c(0,ssaad[i])),col=col[nc],lwd=lw)
        }
    }
}