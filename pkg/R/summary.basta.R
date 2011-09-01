summary.basta <-
function(object,...){
	
	cat("\nCall:\n")
	cat(paste("Model             \t\t: ", object$ModelSpecs[1],"\n",sep=""))
	cat(paste("Shape             \t\t: ", object$ModelSpecs[2],"\n",sep=""))
	cat(paste("Covars. structure \t\t: ", object$ModelSpecs[3],"\n",sep=""))
	cat(paste("Cat. covars.      \t\t: ", object$ModelSpecs[4],"\n",sep=""))
	cat(paste("Cont. covars.     \t\t: ", object$ModelSpecs[5],"\n",collapse=""))

	cat("\nModel settings:\n")
	print(object$set)

	cat("\nRuns:\n")
	id.failed    = which(object$finished==0)
	if(object$set['nsim'] == 1){
		if(length(id.failed) == 0) cat("The simulation finished.\n") else cat("The simulation failed.\n")
	} else {
		if(sum(object$finished)==length(object$finished)) cat("All simulations finished.\n") else if(length(id.failed)==1) cat(paste("Simulation number ", id.failed, " failed.\n", sep="")) else cat(paste("Simulations number ", paste(id.failed[-length(id.failed)], collapse=", ")," and ", id.failed[length(id.failed)], " failed.\n", sep=""))
	}
	
	cat("\nJumps and priors:\n")
	print(object$JumpP)

	cat("\nCoefficients:\n")
	print.default(object$coefficients, ...)
	
	cat("\nConvergence:\n")
	if(is.null(object$ModSel)){
		if(object$set['nsim'] == 1) cat("Convergence calculations require more than one run.\nTo estimate potential scale reduction run at least two simulations.\n") else cat("Not reached for some parameters (i.e. 'PotScaleReduc' values larger than 1.2).\nWarning: These estimates should not be used for inference.\n")
	} else {
		cat("Appropriate convergence reached for all parameters.\n")
	} 
	
	cat("\nModel Selection:\n")
	if(!is.null(object$ModSel)){
		print(object$ModSel)
	} else {
		if(object$set['nsim'] == 1) cat("DIC was not calculated due to insufficient number of simulations to estimate convergence.\n") else cat("DIC was not calculated due to lack of convergence.\n")
	}
	
}

