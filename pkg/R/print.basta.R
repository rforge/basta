print.basta <-
function(x, digits=3){
	catcov    = paste(colnames(x$Za), collapse=", ")
	if(length(colnames(x$Za))==1) catcov = "NULL"
	concov    = paste(colnames(x$Zc), collapse=", ")
	if(length(colnames(x$Zc))==1) concov = "NULL"
	cat("\nCall:\n")
	cat(paste("Model        \t\t", x$ModelSpecs[1],"\n",collapse=""))
	cat(paste("Prop. hazards\t\t", x$ModelSpecs[2],"\n",collapse=""))
	cat(paste("Cat. covars  \t\t", catcov,"\n",collapse=""))
	cat(paste("Cont. covars \t\t", concov,"\n",collapse=""))

	cat("\nRuns:\n")
	id.failed    = which(x$finished==0)
	if(x$set['nsim'] == 1){
		if(length(id.failed) == 0) cat("The simulation finished.\n") else cat("The simulation failed.\n")
	} else {
		if(sum(x$finished)==length(x$finished)) cat("All simulations finished.\n") else if(length(id.failed)==1) cat(paste("Simulation number ", id.failed, " failed.\n", sep="")) else cat(paste("Simulations number ", paste(id.failed[-length(id.failed)], collapse=", ")," and ", id.failed[length(id.failed)], " failed.\n", sep=""))
	}
	
	
	cat("\nCoefficients:\n")
	print.default(x$coefficients, digits=digits)
	if(is.null(x$ModSel)){
		if(x$set['nsim'] == 1) cat("\nConvergence calculations require more than one run.\nTo estimate potential scale reduction run at least two simulations.\n") else cat("\nWarning: Convergence not reached for some parameters (i.e. 'PotScaleReduc' values larger than 1.2).\nThese estimates should not be used for inference.\n")
	} 

}

