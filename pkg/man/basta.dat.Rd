\name{basta.dat}
\alias{basta.dat}
\docType{data}
\title{Simulated dataset for BaSTA analysis}
\description{
This dataset was created by stochastically simulating a hypothetical population with differing mortality patterns between males and females. The population was simulated for 175 years, at each one of which 100 individuals were born. The number of females per generation was randomly drawn from a binomial distribution using function \code{rbinom} with probability of 0.5 (i.e. 1:1 sex ratio). The time of death for each individual was inversed sampled from a Gompertz CDF of ages at death. The Gompertz parameters for females were: \eqn{\alpha = -1} and \eqn{\beta = 0.1}; and for males at \eqn{\alpha = -0.25} and \eqn{\beta = 0.1}. 

The study was assumed to start at year 100 and to finish at year 120. Recapture probability was set at 0.6 and thus each observation per individual was randomly drawn from a Bernoulli trial with parameter \eqn{p = 0.6}. Captures at birth and recoveries were randomly drawn from a Bernoulli trial with parameters \eqn{p = 0.5} and \eqn{p = 0.1}, respectively. 
}
\usage{basta.dat}
\format{RData file, data frame.}
\keyword{datasets}