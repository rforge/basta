\name{designmat}
\alias{designmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
designmat(x, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
