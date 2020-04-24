\name{mvma.hybrid}
\alias{mvma.hybrid}

\title{
Hybrid Model for Random-Effects Multivariate Meta-Analysis
}
\description{
Performs a multivariate meta-analysis using the hybrid random-effects model when the within-study correlations are unknown.
}
\usage{
mvma.hybrid(ys, vars, method = "reml", tol = 1e-10)
}

\arguments{
  \item{ys}{
  an n x p numeric matrix containing the observed effect sizes. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{vars}{
  an n x p numeric matrix containing the observed within-study variances. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{method}{
  a character string indicating the method for estimating the overall effect sizes. It should be \code{"ml"} (random-effects model using the maximum likelihood method) or \code{"reml"} (random-effects model using the restricted maximum likelihood method, the default).
}
  \item{tol}{
  a small number indicating the convergence tolerance for the estimates by maximizing (restricted) likelihood. The default is \code{1e-10}.
}
}
\details{
Suppose \eqn{n} studies are collected in a multivariate meta-analysis on a total of \eqn{p} endpoints. Denote the \eqn{p}-dimensional vector of effect sizes as \eqn{\boldsymbol{y}_i}, and their within-study variances form a diagonal matrix \eqn{\mathbf{D}_i}. However, the within-study correlations are unknown. Then, the random-effects hybrid model is as follows (Riley et al., 2008; Lin and Chu, 2018):
\deqn{\boldsymbol{y}_i \sim N (\boldsymbol{\mu}, (\mathbf{D}_i + \mathbf{T})^{1/2} \mathbf{R} (\mathbf{D}_i + \mathbf{T})^{1/2}),}
where \eqn{\boldsymbol{\mu}} represents the overall effect sizes across studies, \eqn{\mathbf{T} = diag(\tau_1^2, \ldots, \tau_p^2)} consists of the between-study variances, and \eqn{\mathbf{R}} is the marginal correlation matrix. Although the within-study correlations are unknown, this model accounts for both within- and between-study correlations by using the marginal correlation matrix.
}
\value{
This function returns a list containing the following elements:
\item{mu.est}{The estimated overall effect sizes of the p endpoints.}
\item{tau2.est}{The estimated between-study variances of the p endpoints.}
\item{mar.R}{The estimated marginal correlation matrix.}
\item{mu.cov}{The covariance matrix of the estimated overall effect sizes.}
\item{method}{The method used to produce the estimates.}
}
\references{
Lin L, Chu H (2018), "Bayesian multivariate meta-analysis of multiple factors." \emph{Research Synthesis Methods}, \bold{9}(2), 261--272. <\doi{10.1002/jrsm.1293}>

Riley RD, Thompson JR, Abrams KR (2008), "An alternative model for bivariate random-effects meta-analysis when the within-study correlations are unknown." \emph{Biostatistics}, \bold{9}(1), 172--186. <\doi{10.1093/biostatistics/kxm023}>
}

\note{
The algorithm for maximizing (restricted) likelihood may not converge when the dimension of endpoints is too high or the data are too sparse.
}

\seealso{
\code{\link{mvma}}, \code{\link{mvma.bayesian}}, \code{\link{mvma.hybrid.bayesian}}
}
\examples{
\donttest{
data("dat.fib")
y <- dat.fib$y
sd <- dat.fib$sd
mvma.hybrid(y = y, vars = sd^2)
}
}
\keyword{multivariate meta-analysis}