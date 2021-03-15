\name{nma.icdf}
\alias{nma.icdf}
\title{
Evidence Inconsistency Degrees of Freedom in Bayesian Network Meta-Analysis
}
\description{
Calculates evidence inconsistency degrees of freedom (ICDF) in Bayesian network meta-analysis with binary outcomes.
}
\usage{
nma.icdf(sid, tid, r, n, data, type = c("la", "fe", "re"),
         n.adapt = 1000, n.chains = 3, n.burnin = 5000, n.iter = 20000,
         n.thin = 2, traceplot = FALSE, nma.name = NULL, seed = 1234)
}
\arguments{
  \item{sid}{
  a vector specifying the study IDs.
}
  \item{tid}{
  a vector specifying the treatment IDs.
}
  \item{r}{
  a numeric vector specifying the event counts.
}
  \item{n}{
  a numeric vector specifying the sample sizes.
}
  \item{data}{
  an optional data frame containing the network meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{sid}, \code{tid}, \code{r}, and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{type}{
  a character string or a vector of character strings specifying the ICDF measures. It can be chosen from \code{"la"} (the Lu--Ades measure), \code{"fe"} (based on fixed-effects models), and \code{"re"} (based on random-effects models).
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
  \item{traceplot}{
  a logical value indicating whether to generate trace plots of network meta-analysis models for calculating the ICDF measures. It is only used when the argument \code{type} includes \code{"fe"} and/or \code{"re"}.
}
  \item{nma.name}{
  a character string for specifying the name of the network meta-analysis, which will be used in the file names of the trace plots. It is only used when \code{traceplot} = \code{TRUE}.
}
  \item{seed}{
  an integer for specifying the seed of the random number generation for reproducibility during the Markov chain Monte Carlo (MCMC) algorithm for performing Bayesian network meta-analysis models.
}
}
\details{
Network meta-analysis frequenlty assumes that direct evidence is consistent with indirect evidence, but this consistency assumption may not hold in some cases. One may use the ICDF to assess the potential that a network meta-analysis might suffer from inconsistency. Suppose that a network meta-analysis compares a total of \eqn{K} treatments. When it contains two-arm studies only, Lu and Ades (2006) propose to measure the ICDF as
\deqn{ICDF^{LA} = T - K + 1,}
where \eqn{T} is the total number of treatment pairs that are directly compared in the treatment network. This measure is interpreted as the number of independent treatment loops. However, it may not be feasibly calculated when the network meta-analysis contains multi-arm studies. Multi-arm studies provide intrinsically consistent evidence and complicate the calculation of the ICDF.

Alternatively, the ICDF may be measured as the difference between the effective numbers of parameters of the consistency and inconsistency models for network meta-analysis. The consistency model assumes evidence consistency in all treatment loops, and the inconsistency model treats the overall effect sizes of all direct treatment comparisons as separate, unrelated parameters (Dias et al., 2013). The effective number of parameters is frequently used to assess the complexity of Bayesian hierarchical models (Spiegelhalter et al., 2002). The effect measure is the (log) odds ratio in the models used in this function. Let \eqn{p_D^{FE,con}} and \eqn{p_D^{FE,incon}} be the effective numbers of parameters of the consistency and inconsistency models under the fixed-effects setting, and \eqn{p_D^{RE,con}} and \eqn{p_D^{RE,incon}} be those under the random-effects setting. The ICDF measures under the fixed-effects and random-effects settings are
\deqn{ICDF^{FE} = p_D^{FE,incon} - p_D^{FE,con};}
\deqn{ICDF^{RE} = p_D^{RE,incon} - p_D^{RE,con},}
respectively. See more details in Lin (2020).
}
\value{
This function produces a list containing the following results: a table of the number of arms within studies and the corresponding counts of studies (\code{nstudy.trtarm}); the number of multi-arm studies (\code{nstudy.multi}); the set of treatments compared in each multi-arm study (\code{multi.trtarm}); the Lu--Ades ICDF measure (\code{icdf.la}); the ICDF measure based on the fixed-effects consistency and inconsistency models (\code{icdf.fe}); and the ICDF measure based on the random-effects consistency and inconsistency models (\code{icdf.re}). The Lu--Ades ICDF measure is \code{NA} (not available) in the presence of multi-arm studies, because multi-arm studies complicate the counting of independent treatment loops in generic network meta-analyses. When \code{traceplot} = \code{TRUE}, the trace plots will be saved in users' working directory.
}
\references{
Dias S, Welton NJ, Sutton AJ, Caldwell DM, Lu G, Ades AE (2013). "Evidence synthesis for decision making 4: inconsistency in networks of evidence based on randomized controlled trials." \emph{Medical Decision Making}, \bold{33}(5), 641--656. <\doi{10.1177/0272989X12455847}>

Lin L (2020). "Evidence inconsistency degrees of freedom in Bayesian network meta-analysis." \emph{Journal of Biopharmaceutical Statistics}, in press. <\doi{10.1080/10543406.2020.1852247}>

Lu G, Ades AE (2006). "Assessing evidence inconsistency in mixed treatment comparisons." \emph{Journal of the American Statistical Association}, \bold{101}(474), 447--459. <\doi{10.1198/016214505000001302}>

Spiegelhalter DJ, Best NG, Carlin BP, Van Der Linde A (2002). "Bayesian measures of model complexity and fit." \emph{Journal of the Royal Statistical Society, Series B (Statistical Methodology)}, \bold{64}(4), 583--639. <\doi{10.1111/1467-9868.00353}>
}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
\donttest{
data("dat.baker")
## increase n.burnin (e.g., to 50000) and n.iter (e.g., to 200000)
##  for better convergence of MCMC
out <- nma.icdf(sid, tid, r, n, data = dat.baker,
  type = c("la", "fe", "re"),
  n.adapt = 1000, n.chains = 3, n.burnin = 500, n.iter = 2000,
  n.thin = 2, traceplot = FALSE, seed = 1234)
out
}
}
\keyword{multivariate meta-analysis}

