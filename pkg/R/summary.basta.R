summary.basta <-
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

	cat("\nModel settings:\n")
	print(object$set)

	cat("\nJumps and priors:\n")
	print(t(object$JumpP))

	cat("\nCoefficients:\n")
	print.default(object$coefficients, digits=3)
	
	cat("\nModel Selection:\n")
	print(object$ModSel)
	
}

