print.basta <-
function(object, digits=3){
	catcov    = paste(colnames(object$Za), collapse=", ")
	if(length(colnames(object$Za))==1) catcov = "NULL"
	concov    = paste(colnames(object$Zc), collapse=", ")
	if(length(colnames(object$Zc))==1) concov = "NULL"
	cat("\nCall:\n")
	cat(paste("Model        \t\t", object$ModelSpecs[1],"\n",collapse=""))
	cat(paste("Prop. hazards\t\t", object$ModelSpecs[2],"\n",collapse=""))
	cat(paste("Cat. covars  \t\t", catcov,"\n",collapse=""))
	cat(paste("Cont. covars \t\t", concov,"\n",collapse=""))

	cat("\nRuns:\n")
	id.failed    = which(object$finished==0)
	if(sum(object$finished)==length(object$finished)) cat("All simulations finished.\n") else if(length(id.failed)==1) cat(paste("Simulation number ", id.failed, " failed.\n", sep="")) else cat(paste("Simulations number ", paste(id.failed, collapse=", "), " failed.\n", sep=""))
	
	cat("\nCoefficients:\n")
	print.default(signif(object$coefficients, digits=digits))
}

