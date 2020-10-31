\name{mvma}
\alias{mvma}

\title{
Multivariate Meta-Analysis
}
\description{
Performs a multivariate meta-analysis when the within-study correlations are known.
}
\usage{
mvma(ys, covs, method = "reml", tol = 1e-10)
}

\arguments{
  \item{ys}{
  an n x p numeric matrix containing the observed effect sizes. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{covs}{
  a numeric list with length n. Each element is the p x p within-study covariance matrix. \code{NA} is allowed for missing endpoints in the covariance matrix.
}
  \item{method}{
  a character string specifying the method for estimating the overall effect sizes. It should be \code{"fe"} (fixed-effects model), \code{"ml"} (random-effects model using the maximum likelihood method), or \code{"reml"} (random-effects model using the restricted maximum likelihood method, the default).
}
  \item{tol}{
  a small number specifying the convergence tolerance for the estimates by maximizing (restricted) likelihood. The default is \code{1e-10}.
}
}
\details{
Suppose \eqn{n} studies are collected in a multivariate meta-analysis on a total of \eqn{p} endpoints. Denote the \eqn{p}-dimensional vector of effect sizes as \eqn{\boldsymbol{y}_i}, and the within-study covariance matrix \eqn{\mathbf{S}_i} is assumed to be known. Then, the random-effects model is as follows:
\deqn{\boldsymbol{y}_i \sim N (\boldsymbol{\mu}_i, \mathbf{S}_i);}
\deqn{\boldsymbol{\mu}_i \sim N (\boldsymbol{\mu}, \mathbf{T}).}
Here, \eqn{\boldsymbol{\mu}_i} represents the true underlying effect sizes in study \eqn{i}, \eqn{\boldsymbol{\mu}} represents the overall effect sizes across studies, and \eqn{\mathbf{T}} is the between-study covariance matrix due to heterogeneity. By setting \eqn{\mathbf{T} = \mathbf{0}}, this model becomes the fixed-effects model.
}
\value{
This function returns a list containing the following elements:
\item{mu.est}{The estimated overall effect sizes of the p endpoints.}
\item{Tau.est}{The estimated between-study covariance matrix.}
\item{mu.cov}{The covariance matrix of the estimated overall effect sizes.}
\item{method}{The method used to produce the estimates.}
}
\references{
Jackson D, Riley R, White IR (2011). "Multivariate meta-analysis: potential and promise." \emph{Statistics in Medicine}, \bold{30}(20), 2481--2498. <\doi{10.1002/sim.4172}>
}

\seealso{
\code{\link{mvma.bayesian}}, \code{\link{mvma.hybrid}}, \code{\link{mvma.hybrid.bayesian}}
}
\examples{
data("dat.fib")
mvma(ys = dat.fib$y, covs = dat.fib$S, method = "fe")
\donttest{mvma(ys = dat.fib$y, covs = dat.fib$S, method = "reml")}
}
\keyword{multivariate meta-analysis}