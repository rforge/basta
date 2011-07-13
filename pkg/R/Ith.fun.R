Ith.fun <-
function(Z){
	lu          = apply(Z, 2, function(x) length(unique(x)))
	ru          = apply(Z, 2, range)
	idcat       = which(lu==2 & apply(ru,2,sum)==1)
	if(length(idcat)==0) idcat = NULL
	idint       = which(lu==1)
	if(length(idint)==0) idint = NULL
	idcon       = which(lu>2)
	if(length(idcon)==0) idcon = NULL
	
	return(list(int=idint, cat=idcat, cont=idcon))
}

