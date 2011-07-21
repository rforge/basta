print.basta <-
function(object){
	catcov    = paste(colnames(object$Za), collapse=", ")
	if(length(colnames(object$Za))==1) catcov = "NULL"
	concov    = paste(colnames(object$Zc), collapse=", ")
	if(length(colnames(object$Zc))==1) concov = "NULL"
	cat("\nCall:\n")
	cat(paste("Model        \t\t", object$ModelSpecs[1],"\n",collapse=""))
	cat(paste("Prop. hazards\t\t", object$ModelSpecs[2],"\n",collapse=""))
	cat(paste("Cat. covars  \t\t", catcov,"\n",collapse=""))
	cat(paste("Cont. covars \t\t", concov,"\n",collapse=""))

	cat("\nCoefficients:\n")
	print(signif(object$coefficients, 3))
}

