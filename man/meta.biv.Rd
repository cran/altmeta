\name{meta.biv}
\alias{meta.biv}
\title{
Bivariate Method for Meta-Analysis.
}
\description{
Performs a meta-analysis with a binary outcome using a bivariate generalized linear mixed model (GLMM) described in Chu et al.\sspace(2012).
}
\usage{
meta.biv(sid, tid, e, n, data, link = "logit", alpha = 0.05,
         b.iter = 1000, seed = 1234, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sid}{
  a vector specifying the study IDs.
}
  \item{tid}{
  a vector of 0/1 specifying the treatment/exposure IDs (0: control/non-exposure; 1: treatment/exposure).
}
  \item{e}{
  a numeric vector specifying the event counts.
}
  \item{n}{
  a numeric vector specifying the sample sizes.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{sid}, \code{tid}, \code{e}, and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{link}{
  a character string specifying the link function used in the GLMM, which can be either \code{"logit"} (the default) or \code{"probit"}.
}
  \item{alpha}{
  a numeric value specifying the statistical significance level.
}
  \item{b.iter}{
  an integer specifying the number of bootstrap iterations, which are used to produce confidence intervals of marginal results.
}
  \item{seed}{
  an integer for specifying the seed of the random number generation for reproducibility during the bootstrap resampling.
}
  \item{\dots}{
  other arguments that can be passed to the function \code{\link[lme4]{glmer}} in the package \pkg{lme4}.
}
}
\details{
Suppose a meta-analysis with a binary outcome contains \eqn{N} studies. Let \eqn{n_{i0}} and \eqn{n_{i1}} be the sample sizes in the control/non-exposure and treatment/exposure groups in study \eqn{i}, respectively, and let \eqn{e_{i0}} and \eqn{e_{i1}} be the event counts (\eqn{i = 1, \ldots, N}). The event counts are assumed to independently follow binomial distributions:
\deqn{e_{i0} \sim Bin(n_{i0}, p_{i0});}
\deqn{e_{i1} \sim Bin(n_{i1}, p_{i1}),}
where \eqn{p_{i0}} and \eqn{p_{i1}} represent the true event probabilities. They are modeled jointly as follows:
\deqn{g(p_{i0}) = \mu_0 + \nu_{i0};}
\deqn{g(p_{i1}) = \mu_1 + \nu_{i1};}
\deqn{(\nu_{i0}, \nu_{i1})^\prime \sim N ((0, 0)^\prime, \mathbf{\Sigma}).}
Here, \eqn{g(\cdot)} denotes the link function that transforms the event probabilities to linear forms. The fixed effects \eqn{\mu_0} and \eqn{\mu_1} represent the overall event probabilities on the transformed scale. The study-specific parameters \eqn{\nu_{i0}} and \eqn{\nu_{i1}} are random effects, which are assumed to follow the bivariate normal distribution with zero means and variance-covariance matrix \eqn{\mathbf{\Sigma}}. The diagonal elements of \eqn{\mathbf{\Sigma}} are \eqn{\sigma_0^2} and \eqn{\sigma_1^2} (between-study variances due to heterogeneity), and the off-diagonal elements are \eqn{\rho \sigma_0 \sigma_1}, where \eqn{\rho} is the correlation coefficient.

When using the logit link, \eqn{\mu_1 - \mu_0} represents the log odds ratio (Van Houwelingen et al., 1993; Stijnen et al., 2010; Jackson et al., 2018); \eqn{\exp(\mu_1 - \mu_0)} may be referred to as the conditional odds ratio (Agresti, 2013). Alternatively, we can obtain the marginal event probabilities (Chu et al., 2012):
\deqn{p_k = E[p_{ik}] \approx \left[1 + \exp\left(-\mu_k/\sqrt{1 + C^2 \sigma_k^2}\right)\right]^{-1}}
for \eqn{k} = 0 and 1, where \eqn{C = 16 \sqrt{3} / (15 \pi)}. The marginal odds ratio, relative risk, and risk difference are subsequently obtained as \eqn{[p_1/(1 - p_1)]/[p_0/(1 - p_0)]}, \eqn{p_1/p_0}, and \eqn{p_1 - p_0}, respectively.

When using the probit link, the model does not yield the conditional odds ratio. The marginal probabilities have closed-form solutions:
\deqn{p_k = E[p_{ik}] = \Phi\left(\mu_k/\sqrt{1 + \sigma_k^2}\right)}
for \eqn{k} = 0 and 1, where \eqn{\Phi(\cdot)} is the cumulative distribution function of the standard normal distribution. They further lead to the marginal odds ratio, relative risk, and risk difference.

}
\value{
This function returns a list containing the point and interval estimates of the marginal event rates (\code{p0.m}, \code{p0.m.ci}, \code{p1.m}, and \code{p1.m.ci}), odds ratio (\code{OR.m} and \code{OR.m.ci}), relative risk (\code{RR.m} and \code{RR.m.ci}), risk difference (\code{RD.m} and \code{RD.m.ci}), and correlation coefficient between the two treatment/exposure groups (\code{rho} and \code{rho.ci}). These interval estimates are obtained using the bootstrap resampling. During the bootstrap resampling, computational warnings or errors may occur for implementing the bivariate GLMM in some resampled meta-analyses. This function returns the counts of warnings and errors (\code{b.w.e}). The resampled meta-analyses that lead to warnings and errors are not used for producing the bootstrap confidence intervals; the bootstrap iterations stop after obtaining \code{b.iter} resampled meta-analyses without warnings and errors. If the logit link is used (\code{link} = \code{"logit"}), it also returns the point and interval estimates of the conditional odds ratio (\code{OR.c} and \code{OR.c.ci}), which are more frequently reported in the current literature than the marginal odds ratios. Unlike the marginal results that use the bootstrap resampling to produce their confidence intervals, the Wald-type confidence interval is calculated for the log conditional odds ratio; it is then transformed to the odds ratio scale.
}
\references{
Agresti A (2013). \emph{Categorical Data Analysis}. Third edition. John Wiley & Sons, Hoboken, NJ.

Chu H, Nie L, Chen Y, Huang Y, Sun W (2012). "Bivariate random effects models for meta-analysis of comparative studies with binary outcomes: methods for the absolute risk difference and relative risk." \emph{Statistical Methods in Medical Research}, \bold{21}(6), 621--633. <\doi{10.1177/0962280210393712}>

Jackson D, Law M, Stijnen T, Viechtbauer W, White IR (2018). "A comparison of seven random-effects models for meta-analyses that estimate the summary odds ratio." \emph{Statistics in Medicine}, \bold{37}(7), 1059--1085. <\doi{10.1002/sim.7588}>

Stijnen T, Hamza TH, Ozdemir P (2010). "Random effects meta-analysis of event outcome in the framework of the generalized linear mixed model with applications in sparse data." \emph{Statistics in Medicine}, \bold{29}(29), 3046--3067. <\doi{10.1002/sim.4040}>

Van Houwelingen HC, Zwinderman KH, Stijnen T (1993). "A bivariate approach to meta-analysis." \emph{Statistics in Medicine}, \bold{12}(24), 2273--2284. <\doi{10.1002/sim.4780122405}>
}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maprop.glmm}}, \code{\link{meta.dt}}
}
\examples{
\donttest{
data("dat.bellamy")
out.bellamy.logit <- meta.biv(sid, tid, e, n, data = dat.bellamy,
  link = "logit", b.iter = 1000)
out.bellamy.logit
out.bellamy.probit <- meta.biv(sid, tid, e, n, data = dat.bellamy,
  link = "probit", b.iter = 1000)
out.bellamy.probit
}
}
\keyword{generalized linear mixed model}
