\name{plot.traces}
\alias{plot.traces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.traces(outBS, tracename = "theta")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outBS}{
%%     ~~Describe \code{outBS} here~~
}
  \item{tracename}{
%%     ~~Describe \code{tracename} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(outBS, tracename = "theta"){
	require(RColorBrewer)
	if(.Platform$OS.type=="unix") devtype=quartz else devtype=windows
	plotname     = c("theta","gamma","pi", "post")
	if(!is.element(tracename, plotname)) stop(paste("Wrong 'tracename' argument. Valid arguments are:", paste(paste("'",plotname,"'", sep=""), collapse=", "),".\n"), call.=FALSE)
	
	if(tracename=="gamma" & is.na(max(outBS$results$gamma))) stop("\nTrace plots cannot be drawn for 'gamma' parameters.\nNo proportional hazards arguments were evaluated.", call.=FALSE)

	# PLOTS:
	# Trace plots for parameters:
	p           = which(plotname == tracename)
	nza         = ncol(outBS$data$Za)
	nzc         = ncol(outBS$data$Zc)
	ydim        = c(sum(outBS$input$modm[outBS$input$idm,]), ceiling(nzc/2), ceiling((ncol(outBS$results$pi))/2), 2)
	xdim        = c(nza, 2, 2, 2)

	x           = outBS$results[[tracename]]
	Main        = list(theta = colnames(outBS$results$theta), gamma=colnames(outBS$results$gamma), pi = colnames(outBS$results$pi), post = expression(paste(p,"(",theta," | ",X,")"), paste(p,"(",X[0]," | ",X[1],",",theta,",",pi,",",")"), paste(p,"(",X[0],",",theta,",",pi,"| ... ",")")))
	if(ncol(x)==1 & tracename=="pi"){
		Main$pi = "pi"
		ydim[2] = 1
		xdim[2] = 1
	} 

	par(mfrow=c(ydim[p], xdim[p]), mar=c(3,3,2,1))
	for(i in 1:ncol(x)){
		
		if(length(dim(x))<3){
			plot(x[,i], type='l', xlab="Iteration", ylab="", main=Main[[p]][i], frame.plot=FALSE)
		} else {
			yl    = range(x[,i,], na.rm=TRUE)
			plot(c(1,nrow(x)), yl, col=NA, xlab="Iteration", ylab="", main=Main[[p]][i], frame.plot=FALSE)
			for(j in 1:dim(x)[3]) lines(x[,i,j], type='l', col=brewer.pal(12, "Paired")[j])
		}
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
