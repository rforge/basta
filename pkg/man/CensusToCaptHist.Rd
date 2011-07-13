\name{CensusToCaptHist}
\alias{CensusToCaptHist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CensusToCaptHist(ID, d, dformat = "dd/mm/yyyy")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{
%%     ~~Describe \code{ID} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{dformat}{
%%     ~~Describe \code{dformat} here~~
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
function(ID,d,dformat="dd/mm/yyyy"){

# Check data
if(class(ID) != "character") ID = as.character(ID)


if(class(d) == "Date") {yr = as.numeric(format(d, format = "\%Y")) }



if(class(d) != "Date"){
    # Deduce the d format, if it is not already a d object
    st = strsplit(dformat, split = NULL)
    y  = which(st[[1]]=="y")

    # Extract the year
    yr = as.numeric(substr(d,y[1],y[length(y)]))
    }


# Add a dummy individual seen in all years, 
# to cope with years where there are no recaptures
dyr = min(yr):max(yr)
ID  = c(ID,rep("XdummyX",length(dyr)))
yr  = c(yr,dyr)

mat        = as.matrix(table(ID,yr))
mat[mat>0] = 1

#Remove the dummy row
mat        = mat[-which(rownames(mat)=="XdummyX"),]
return(mat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
