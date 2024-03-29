\name{mvma.bayesian}
\alias{mvma.bayesian}

\title{
Bayesian Random-Effects Multivariate Meta-Analysis
}
\description{
Performs a Bayesian random-effects model for multivariate meta-analysis when the within-study correlations are known.
}
\usage{
mvma.bayesian(ys, covs, data, n.adapt = 1000, n.chains = 3,
              n.burnin = 10000, n.iter = 10000, n.thin = 1,
              data.name = NULL, traceplot = FALSE, coda = FALSE)
}

\arguments{
  \item{ys}{
  an n x p numeric matrix containing the observed effect sizes. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{covs}{
  a numeric list with length n. Each element is the p x p within-study covariance matrix. \code{NA} is allowed for missing endpoints in the covariance matrix.
}
  \item{data}{
  an optional data frame containing the multivariate meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{ys} and \code{covs}, should be specified as their corresponding column names in \code{data}.
}
  \item{n.adapt}{
  the number of iterations for adaptation in the Markov chain Monte Carlo (MCMC) algorithm. The default is 1,000. This argument and the following \code{n.chains}, \code{n.burnin}, \code{n.iter}, and \code{n.thin} are passed to the functions in the package \pkg{rjags}.
}
  \item{n.chains}{
  the number of MCMC chains. The default is 3.
}
  \item{n.burnin}{
  the number of iterations for burn-in period. The default is 10,000.
}
  \item{n.iter}{
  the total number of iterations in each MCMC chain after the burn-in period. The default is 10,000.
}
  \item{n.thin}{
  a positive integer specifying thinning rate. The default is 1.
}
  \item{data.name}{
  a character string specifying the data name. This is used in the names of the generated files that contain results. The default is \code{NULL}.
}
  \item{traceplot}{
  a logical value indicating whether to save trace plots for the overall effect sizes and between-study standard deviations. The default is \code{FALSE}.
}
  \item{coda}{
  a logical value indicating whether to output MCMC posterior samples. The default is \code{FALSE}.
}
}
\details{
Suppose \eqn{n} studies are collected in a multivariate meta-analysis on a total of \eqn{p} endpoints. Denote the \eqn{p}-dimensional vector of effect sizes as \eqn{\boldsymbol{y}_i}, and the within-study covariance matrix \eqn{\mathbf{S}_i} is assumed to be known. Then, the random-effects model is as follows:
\deqn{\boldsymbol{y}_i \sim N (\boldsymbol{\mu}_i, \mathbf{S}_i);}
\deqn{\boldsymbol{\mu}_i \sim N (\boldsymbol{\mu}, \mathbf{T}).}
Here, \eqn{\boldsymbol{\mu}_i} represents the true underlying effect sizes in study \eqn{i}, \eqn{\boldsymbol{\mu}} represents the overall effect sizes across studies, and \eqn{\mathbf{T}} is the between-study covariance matrix due to heterogeneity.

The vague priors \eqn{N (0, 10^3)} are specified for the fixed effects \eqn{\boldsymbol{\mu}}. Also, this function uses the separation strategy to specify vague priors for the variance and correlation components in \eqn{\mathbf{T}} (Pinheiro and Bates, 1996); this technique is considered less sensitive to hyperparameters compared to specifying the inverse-Wishart prior (Lu and Ades, 2009; Wei and Higgins, 2013). Specifically, write the between-study covariance matrix as \eqn{\mathbf{T} = \mathbf{D}^{1/2} \mathbf{R} \mathbf{D}^{1/2}}, where the diagonal matrix \eqn{\mathbf{D} = diag(\mathbf{T}) = diag(\tau_1^2, \ldots, \tau_p^2)} contains the between-study variances, and \eqn{\mathbf{R}} is the correlation matrix. Uniform priors \eqn{U (0, 10)} are specified for \eqn{\tau_j}'s (\eqn{j = 1, \ldots, p}). Further, the correlation matrix can be written as \eqn{\mathbf{R} = \mathbf{L} \mathbf{L}^\prime}, where \eqn{\mathbf{L} = (L_{ij})} is a lower triangular matrix with nonnegative diagonal elements. Also, \eqn{L_{11} = 1} and for \eqn{i = 2, \ldots, p}, \eqn{L_{ij} = \cos \theta_{i2}} if \eqn{j = 1}; \eqn{L_{ij} = (\prod_{k = 2}^{j} \sin \theta_{ik}) \cos \theta_{i, j + 1}} if \eqn{j = 2, \ldots, i - 1}; and \eqn{L_{ij} = \prod_{k = 2}^{i} \sin \theta_{ik}} if \eqn{j = i}. Here, \eqn{\theta_{ij}}'s are angle parameters for \eqn{2 \leq j \leq i \leq p}, and \eqn{\theta_{ij} \in (0, \pi)}. Uniform priors are specified for the angle parameters: \eqn{\theta_{ij} \sim U (0, \pi)}.
}
\value{
This functions produces posterior estimates and Gelman and Rubin's potential scale reduction factor, and it generates several files that contain trace plots (if \code{traceplot} = \code{TRUE}) and MCMC posterior samples (if \code{coda} = \code{TRUE}) in users' working directory. In these results, \code{mu} represents the overall effect sizes, \code{tau} represents the between-study variances, \code{R} contains the elements of the correlation matrix, and \code{theta} represents the angle parameters (see "Details").
}
\references{
Gelman A, Rubin DB (1992). "Inference from iterative simulation using multiple sequences." \emph{Statistical Science}, \bold{7}(4), 457--472. <\doi{10.1214/ss/1177011136}>

Jackson D, Riley R, White IR (2011). "Multivariate meta-analysis: potential and promise." \emph{Statistics in Medicine}, \bold{30}(20), 2481--2498. <\doi{10.1002/sim.4172}>

Lu G, Ades AE (2009). "Modeling between-trial variance structure in mixed treatment comparisons." \emph{Biostatistics}, \bold{10}(4), 792--805. <\doi{10.1093/biostatistics/kxp032}>

Pinheiro JC, Bates DM (1996). "Unconstrained parametrizations for variance-covariance matrices." \emph{Statistics and Computing}, \bold{6}(3), 289--296. <\doi{10.1007/BF00140873}>

Wei Y, Higgins JPT (2013). "Bayesian multivariate meta-analysis with multiple outcomes." \emph{Statistics in Medicine}, \bold{32}(17), 2911--2934. <\doi{10.1002/sim.5745}>
}

\note{
This function only implements the MCMC algorithm for the random-effects multivariate model, but not the fixed-effects model. Generally, the fixed-effects model can be easily implemented using the function \code{\link{mvma}}. However, when using \code{\link{mvma}} to fit the random-effects model, a large number of parameters need to be estimated, and the algorithm for maximizing (restricted) likelihood may not converge well. The Bayesian method in this function provides an alternative.

If a warning "adaptation incomplete" appears, users may increase \code{n.adapt}.
}

\seealso{
\code{\link{mvma}}, \code{\link{mvma.hybrid}}, \code{\link{mvma.hybrid.bayesian}}
}
\examples{
\donttest{
data("dat.fib")
set.seed(12345)
## increase n.burnin and n.iter for better convergence of MCMC
out <- mvma.bayesian(ys = y, covs = S, data = dat.fib,
  n.adapt = 1000, n.chains = 3, n.burnin = 100, n.iter = 100,
  n.thin = 1, data.name = "Fibrinogen")
out
}
}
\keyword{multivariate meta-analysis}