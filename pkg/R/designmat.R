designmat <-
function(x, data){
	if(is.null(x)){
		covmat   = matrix(1, nrow(data))
	} else {
		if(is.character(x)){
			idncov   = which(!is.element(x, colnames(data)))
			if(length(idncov)>0){
				stop("Some arguments in 'x' do not match the column names in data frame 'Data'.\n", call.=FALSE)
			} else {
				covs   = as.formula(paste("~", paste(x, collapse="+"),sep=""))
			}
		} else if(is.numeric(x)){
			idncov   = which(!is.element(x, 1:ncol(data)))
			if(length(idncov)>0){
				stop("Some arguments in 'x' do not match the column numbers in data frame 'Data'.\n", call.=FALSE)
			} else {
				covs   = as.formula(paste("~", paste(colnames(data[,x]), collapse="+"),sep=""))
			}
		} else if(class(x)=="formula"){
			xcov    = labels(terms(x))
			xcov    = unique(unlist(strsplit(xcov, split=":")))
			idncov  = which(!is.element(xcov, colnames(data)))
			if(length(idncov)>0){
				stop("Some variables in 'x' do not match the column names in data.\n")
			} else {
				covs = x
			}
		}
		covmat   = cbind(model.matrix(covs, data=data))
	}
	return(covmat)
}

