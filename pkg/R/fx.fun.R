fx.fun <-
function(x,th, ga=0, idm=3, log=FALSE){
	fx       = mx.fun(x, th, ga, idm=idm, log=TRUE) + Sx.fun(x, th, ga, idm=idm, log=TRUE)
	if(!log) fx = exp(fx)
	return(fx)
}

