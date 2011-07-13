\name{makecovm}
\alias{makecovm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makecovm(Data, covs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{covs}{
%%     ~~Describe \code{covs} here~~
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
function(Data,covs) {

nd = data.frame(Data[,which(names(Data)\%in\%covs)])
if(ncol(nd)==1) names(nd)=covs

Cl = as.vector(sapply(nd,class))

temp = c(Cl == "factor"|Cl == "numeric")
if(sum(temp)<length(Cl)) stop("Data types for the covariates should be 'factor' or 'numeric'")

factorCovs  = which(Cl=="factor")
numericCovs = which(Cl=="numeric")

if(length(factorCovs) >  1)  FactCovs = as.factor(apply(nd[,factorCovs],1,paste,collapse="."))
if(length(factorCovs) == 1) FactCovs = nd[,factorCovs]
if(length(factorCovs) == 0) FactCovs = NULL

if(length(numericCovs) != 0) NumCovs = nd[,which(Cl=="numeric")]
if(length(numericCovs) == 0) NumCovs = NULL

if(is.null(FactCovs)) {tempCovs = data.frame(NumCovs)} 
if(is.null(NumCovs)) {tempCovs = data.frame(FactCovs)}
if(!is.null(NumCovs)&!is.null(FactCovs)) {tempCovs = data.frame(FactCovs,NumCovs)}

Z=NULL

for (x in 1:ncol(tempCovs)){
    if(class(tempCovs[,x]) == "numeric"){
        Z = cbind(Z,tempCovs[,x])
        }
    
    if(class(tempCovs[,x]) == "factor"){
        vL = length(tempCovs[,x])
        nLevs = length(levels(tempCovs[,x]))
        a = (vL * nLevs) - nLevs
        sv = seq(0, a, nLevs) + as.numeric(tempCovs[,x])
        MV = rep(0, vL * nLevs)
        MV[sv] = 1
        M = matrix(MV, ncol = nLevs, nrow = vL, byrow = TRUE)
        colnames(M) = levels(tempCovs[,x])
        
        Z = cbind(Z,M)
        }
    }

#Assign column names to the numeric variables (if there are any)
if(length(numericCovs)!=0){
    if(is.null(colnames(Z))) {colnames(Z) = rep("",length(numericCovs))}
    colnames(Z)[colnames(Z)==""] = names(nd)[Cl=="numeric"]
    }

return(Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
