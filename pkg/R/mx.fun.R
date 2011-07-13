mx.fun <-
function(x,th, ga=0, idm=3, log=FALSE){
	mx    = th[,3] + exp(th[,4] + th[,5]*x)
	if(idm==3) mx = mx + exp(th[,1]-th[,2]*x)
	mx    = mx * exp(ga)
	if(log) mx = log(mx)
	return(mx)
}

