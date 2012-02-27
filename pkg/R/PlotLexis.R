PlotLexis <- function(x, ji = 0.5, trans=50, q = 0.5, lw = 2, ...){

  #Define colours
Palette = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
                         "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
                         "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
if(trans != "NULL") Palette = paste(Palette,trans,sep="")

  #Birth time estimate
  BirthTime  = x$Bis
  
  #Death time estimate
  DeathTime = x$Dis

  #Age at death estimate, calculated from birth and death times
  AgeAtDeath = DeathTime - BirthTime

  #Calculate 95% intervals for Age At Death (AAD) and Birth Time (BT) estimates
  BTq  = apply(BirthTime, 2, quantile, q)
  DTq  = apply(DeathTime, 2, quantile, q)

  nCat       = ncol(x$Zcat)
  CatNames   = colnames(x$Zcat)
  
  par(mfrow=c(nCat,1))
  for(nc in 1:nCat){
    plot(c(min(BTq),max(BTq)),c(0,max(AgeAtDeath)),type="n",xlim=c(min(BTq),max(DTq)),...)
    title(main=CatNames[nc])
    
    #Subset the birthtimes/deathtimes for the nc Categorical variable.
    ssbt  = BTq[x$Zcat[,nc]==1]
    ssdt  = DTq[x$Zcat[,nc]==1]
    ssaad = ssdt-ssbt
  
    #Plot a single line for each individual, jittering as necessary
    for (i in 1:length(ssbt)){
        jitterval = runif(1,0,1)*ji
        lines(c(ssbt[i]+jitterval,ssdt[i]+jitterval),(c(0,ssaad[i])),col=Palette[nc],lwd=lw)
        }
    }

#Report basic birth/death information - useful for checking whether this is done correctly
cat(paste("Earliest birth:",min(BTq)))
cat(paste("\nLatest birth:",max(BTq)))
cat(paste("\nEarliest death:",min(DTq)))
cat(paste("\nLatest death:",max(DTq)))

    } 