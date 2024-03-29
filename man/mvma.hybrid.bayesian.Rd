\name{mvma.hybrid.bayesian}
\alias{mvma.hybrid.bayesian}

\title{
Bayesian Hybrid Model for Random-Effects Multivariate Meta-Analysis
}
\description{
Performs a multivariate meta-analysis using the Bayesian hybrid random-effects model when the within-study correlations are unknown.
}
\usage{
mvma.hybrid.bayesian(ys, vars, data, n.adapt = 1000, n.chains = 3,
                     n.burnin = 10000, n.iter = 10000, n.thin = 1,
                     data.name = NULL, traceplot = FALSE, coda = FALSE)
}

\arguments{
  \item{ys}{
  an n x p numeric matrix containing the observed effect sizes. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{vars}{
  an n x p numeric matrix containing the observed within-study variances. The n rows represent studies, and the p columns represent the multivariate endpoints. \code{NA} is allowed for missing endpoints.
}
  \item{data}{
  an optional data frame containing the multivariate meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{ys} and \code{vars}, should be specified as their corresponding column names in \code{data}.
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
Suppose \eqn{n} studies are collected in a multivariate meta-analysis on a total of \eqn{p} endpoints. Denote the \eqn{p}-dimensional vector of effect sizes as \eqn{\boldsymbol{y}_i}, and their within-study variances form a diagonal matrix \eqn{\mathbf{D}_i}. However, the within-study correlations are unknown. Then, the random-effects hybrid model is as follows (Riley et al., 2008; Lin and Chu, 2018):
\deqn{\boldsymbol{y}_i \sim N (\boldsymbol{\mu}, (\mathbf{D}_i + \mathbf{T})^{1/2} \mathbf{R} (\mathbf{D}_i + \mathbf{T})^{1/2}),}
where \eqn{\boldsymbol{\mu}} represents the overall effect sizes across studies, \eqn{\mathbf{T} = diag(\tau_1^2, \ldots, \tau_p^2)} consists of the between-study variances, and \eqn{\mathbf{R}} is the marginal correlation matrix. Although the within-study correlations are unknown, this model accounts for both within- and between-study correlations by using the marginal correlation matrix.

Uniform priors \eqn{U (0, 10)} are specified for the between-study standard deviations \eqn{\tau_j} (\eqn{j = 1, \ldots, p}). The correlation matrix can be written as \eqn{\mathbf{R} = \mathbf{L} \mathbf{L}^\prime}, where \eqn{\mathbf{L} = (L_{ij})} is a lower triangular matrix with nonnegative diagonal elements. Also, \eqn{L_{11} = 1} and for \eqn{i = 2, \ldots, p}, \eqn{L_{ij} = \cos \theta_{i2}} if \eqn{j = 1}; \eqn{L_{ij} = (\prod_{k = 2}^{j} \sin \theta_{ik}) \cos \theta_{i, j + 1}} if \eqn{j = 2, \ldots, i - 1}; and \eqn{L_{ij} = \prod_{k = 2}^{i} \sin \theta_{ik}} if \eqn{j = i} (Lu and Ades, 2009; Wei and Higgins, 2013). Here, \eqn{\theta_{ij}}'s are angle parameters for \eqn{2 \leq j \leq i \leq p}, and \eqn{\theta_{ij} \in (0, \pi)}. Uniform priors are specified for the angle parameters: \eqn{\theta_{ij} \sim U (0, \pi)}.
}
\value{
This functions produces posterior estimates and Gelman and Rubin's potential scale reduction factor, and it generates several files that contain trace plots (if \code{traceplot} = \code{TRUE}), and MCMC posterior samples (if \code{coda} = \code{TRUE}) in users' working directory. In these results, \code{mu} represents the overall effect sizes, \code{tau} represents the between-study variances, \code{R} contains the elements of the correlation matrix, and \code{theta} represents the angle parameters (see "Details").
}
\references{
Lin L, Chu H (2018), "Bayesian multivariate meta-analysis of multiple factors." \emph{Research Synthesis Methods}, \bold{9}(2), 261--272. <\doi{10.1002/jrsm.1293}>

Lu G, Ades AE (2009). "Modeling between-trial variance structure in mixed treatment comparisons." \emph{Biostatistics}, \bold{10}(4), 792--805. <\doi{10.1093/biostatistics/kxp032}>

Riley RD, Thompson JR, Abrams KR (2008), "An alternative model for bivariate random-effects meta-analysis when the within-study correlations are unknown." \emph{Biostatistics}, \bold{9}(1), 172--186. <\doi{10.1093/biostatistics/kxm023}>

Wei Y, Higgins JPT (2013). "Bayesian multivariate meta-analysis with multiple outcomes." \emph{Statistics in Medicine}, \bold{32}(17), 2911--2934. <\doi{10.1002/sim.5745}>
}

\seealso{
\code{\link{mvma}}, \code{\link{mvma.bayesian}}, \code{\link{mvma.hybrid}}
}
\examples{
\donttest{
data("dat.pte")
set.seed(12345)
## increase n.burnin and n.iter for better convergence of MCMC
out <- mvma.hybrid.bayesian(ys = dat.pte$y, vars = (dat.pte$se)^2,
  n.adapt = 1000, n.chains = 3, n.burnin = 100, n.iter = 100,
  n.thin = 1, data.name = "Pterygium")
out
}
}
\keyword{multivariate meta-analysis}