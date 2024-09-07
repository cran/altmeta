\name{meta.or.smd}
\alias{meta.or.smd}
\title{
Meta-Analysis of Combining Standardized Mean Differences and Odds Ratios
}
\description{
Performs a Bayesian meta-analysis to synthesize standardized mean differences (SMDs) for a continuous outcome and odds ratios (ORs) for a binary outcome.
}
\usage{
meta.or.smd(y1, sd1, n1, y0, sd0, n0, r1, m1, r0, m0, id.bin, data,
            n.adapt = 1000, n.chains = 3, n.burnin = 5000, n.iter = 20000, n.thin = 2,
            seed = 1234)
}
\arguments{
  \item{y1}{
  a vector specifying the sample means in the treatment group for the continuous outcome. \code{NA} is allowed and indicates that data are not available for this outcome; the same applies to the other arguments, including \code{sd1}, \code{n1}, \code{y0}, \code{sd0}, \code{n0}, \code{r1}, \code{m1}, \code{r0}, and \code{m0}.
}
  \item{sd1}{
  a vector specifying the sample standard deviations in the treatment group for the continuous outcome.
}
  \item{n1}{
  a vector specifying the sample sizes in the treatment group for the continuous outcome.
}
  \item{y0}{
  a vector specifying the sample means in the control group for the continuous outcome.
}
  \item{sd0}{
  a vector specifying the sample standard deviations in the control group for the continuous outcome.
}
  \item{n0}{
  a vector specifying the sample sizes in the control group for the continuous outcome.
}
  \item{r1}{
  a vector specifying the event counts in the treatment group for the binary outcome.
}
  \item{m1}{
  a vector specifying the sample sizes in the treatment group for the binary outcome.
}
  \item{r0}{
  a vector specifying the event counts in the control group for the binary outcome.
}
  \item{m0}{
  a vector specifying the sample sizes in the control group for the binary outcome.
}
  \item{id.bin}{
  a vector indicating whether the outcome is binary (1) or continuous (0).
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{y1}, \code{sd1}, \code{n1}, \code{y0}, \code{sd0}, \code{n0}, \code{r1}, \code{m1}, \code{r0}, \code{m0}, and \code{id.bin} should be specified as their corresponding column names in \code{data}.
}
  \item{n.adapt}{
  the number of iterations for adaptation in the Markov chain Monte Carlo (MCMC) algorithm. The default is 1,000. This argument and the following \code{n.chains}, \code{n.burnin}, \code{n.iter}, and \code{n.thin} are passed to the functions in the package \pkg{rjags}.
}
  \item{n.chains}{
  the number of MCMC chains. The default is 3.
}
  \item{n.burnin}{
  the number of iterations for burn-in period. The default is 5,000.
}
  \item{n.iter}{
  the total number of iterations in each MCMC chain after the burn-in period. The default is 20,000.
}
  \item{n.thin}{
  a positive integer specifying thinning rate. The default is 2.
}
  \item{seed}{
  an integer for specifying the seed of the random number generation for reproducibility during the MCMC algorithm for performing the Bayesian meta-analysis model.
}
}
\details{
The Bayesian meta-analysis model implemented by this function is detailed in Section 2.5 of Jing et al.\sspace(2023).
}
\value{
"This function returns a list of Bayesian estimates, including posterior medians and 95\% credible intervals (comprising the 2.5\% and 97.5\% posterior quantiles) for the overall SMD (\code{d}), the between-study standard deviation (\code{tau}), and the individual studies' SMDs (\code{theta}).
}
\references{
Jing Y, Murad MH, Lin L (2023). "A Bayesian model for combining standardized mean differences and odds ratios in the same meta-analysis." \emph{Journal of Biopharmaceutical Statistics}, \bold{33}(2), 167--190. <\doi{10.1080/10543406.2022.2105345}>
}
\author{
Yaqi Jing, Lifeng Lin 
}

\examples{
\donttest{
data("dat.dep")
out <- meta.or.smd(y1, sd1, n1, y0, sd0, n0, r1, m1, r0, m0, id.bin, data = dat.dep)
out
}
}
\keyword{multivariate meta-analysis}
