\name{basta.dat}
\alias{basta.dat}
\docType{data}
\title{Simulated dataset for BaSTA analysis}
\description{
This dataset was created by stochastically simulating a hypothetical population with differing mortality patterns between males and females and with proportional decreases in mortality as a function of a hypothetical continuous covariate (e.g. birth weight, average adult weight, etc.). The population was simulated for 175 years, at each one of which 100 individuals were born. The number of females per generation was randomly drawn from a binomial distribution using function \code{rbinom} with probability of 0.5 (i.e. 1:1 sex ratio). The individual continuous covariate was randomly drawn from a random normal distribution (with function \code{\link{rnorm}}) with mean parameter equal to 10 and standard deviation equal to 1.  The time of death for each individual was inversed sampled from a Gompertz CDF of ages at death. The Gompertz parameters for females were: \eqn{\alpha = -1} and \eqn{\beta = 0.05}; and for males at \eqn{\alpha = -0.5} and \eqn{\beta = 0.075}. The gamma parameter for the continuous covariate was \eqn{\gamma = 0.1}.

The study was assumed to start at year 100 and to finish at year 120. Recapture probability was set at 0.6 and thus each observation per individual was randomly drawn from a Bernoulli trial with parameter \eqn{p = 0.6}. Captures at birth and recoveries were randomly drawn from a Bernoulli trial with parameters \eqn{p = 0.5} and \eqn{p = 0.1}, respectively. 
}
\usage{basta.dat}
\format{RData file, data frame.}
\keyword{datasets}