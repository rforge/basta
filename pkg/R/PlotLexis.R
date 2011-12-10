PlotLexis <- function(x,ci=1,ji=0.5,col="#007FFF15",xlab="Year",ylab="Age (yrs)"){

BirthTime = x$Bq[ci,]
AgeAtDeath = x$Xq[ci,]
DeathTime = BirthTime+AgeAtDeath

plot(c(min(BirthTime),max(BirthTime)),c(0,max(AgeAtDeath)),type="n",axes=T,xlab=xlab,ylab=ylab,xlim=c(min(BirthTime),max(DeathTime)))

for (i in 1:length(BirthTime)){
jitterval = runif(1,0,1)*ji
lines(c(BirthTime[i]+jitterval,DeathTime[i]+jitterval),(c(0,AgeAtDeath[i])),col=col,lwd=4)
}
}