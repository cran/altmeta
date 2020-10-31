\name{maprop.glmm}
\alias{maprop.glmm}
\title{
Meta-Analysis of Proportions Using Generalized Linear Mixed Models
}
\description{
Performs a meta-analysis of proportions using generalized linear mixed models (GLMMs) with various link functions.
}
\usage{
maprop.glmm(e, n, data, link = "logit", alpha = 0.05,
            pop.avg = TRUE, int.approx = 10000, b.iter = 1000,
            seed = 1234, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{
  a numeric vector specifying the event counts in the collected studies.
}
  \item{n}{
  a numeric vector specifying the sample sizes in the collected studies.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{e} and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{link}{
  a character string specifying the link function used in the GLMM, which can be one of \code{"log"} (log link), \code{"logit"} (logit link, the default), \code{"probit"} (probit link), \code{"cauchit"} (cauchit link), and \code{"cloglog"} (complementary log-log link).
}
  \item{alpha}{
  a numeric value specifying the statistical significance level.
}
  \item{pop.avg}{
  a logical value indicating whether the population-averaged proportion and its confidence interval are to be produced. This quantity is the marginal mean of study-specific proportions, while the commonly-reported overall proportion usually represents the median (or interpreted as a conditional measure); see more details about this quantity in Section 13.2.3 in Agresti (2013), Chu et al.{\sspace}(2012), Lin and Chu (2020), and Zeger et al.{\sspace}(1988). If \code{pop.avg} = \code{TRUE} (the default), the bootstrap resampling is used to produce the confidence interval of the population-averaged proportion; the confidence interval of the commonly-reported median proportion will be also produced, in addition to its conventional confidence interval (by back-transforming the Wald-type confidence interval derived on the scale specified by \code{link}).
}
  \item{int.approx}{
  an integer specifying the number of independent standard normal samples for numerically approximating the integration involved in the calculation of the population-averaged proportion; see details in Lin and Chu (2020). It is only used when \code{pop.avg} = \code{TRUE} and \code{link} is not \code{"probit"}. The probit link leads to a closed form of the population-averaged proportion, so it does not need the numerical approximation; for other links, the population-averaged proportion does not have a closed form.
}
  \item{b.iter}{
  an integer specifying the number of bootstrap iterations; it is only used when \code{pop.avg} = \code{TRUE}.
}
  \item{seed}{
  an integer for specifying the seed of the random number generation for reproducibility during the bootstrap resampling (and numerical approximation for the population-averaged proportion); it is only used when \code{pop.avg} = \code{TRUE}.
}
  \item{...}{
  other arguments that can be passed to the function \code{\link[lme4]{glmer}} in the package \pkg{lme4}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function returns a list containing the point and interval estimates of the overall proportion. Specifically, \code{prop.c.est} is the commonly-reported median (or conditional) proportion, and \code{prop.c.ci} is its confidence interval. It also returns information about AIC, BIC, log likelihood, deviance, and residual degrees-of-freedom. If \code{pop.avg} = \code{TRUE}, the following additional elements will be also in the produced list: \code{prop.c.ci.b} is the bootstrap confidence interval of the commonly-reported median (conditional) proportion, \code{prop.m.est} is the point estimate of the population-averaged (marginal) proportion, \code{prop.m.ci.b} is the bootstrap confidence interval of the population-averaged (marginal) proportion, and \code{b.w.e} is a vector of two numeric values, indicating the counts of warnings and errors occurred during the bootstrap iterations.
}
\references{
Agresti A (2013). \emph{Categorical Data Analysis}. Third edition. John Wiley & Sons, Hoboken, NJ.

Bakbergenuly I, Kulinskaya E (2018). "Meta-analysis of binary outcomes via generalized linear mixed models: a simulation study." \emph{BMC Medical Research Methodology}, \bold{18}, 70. <\doi{10.1186/s12874-018-0531-9}>

Chu H, Nie L, Chen Y, Huang Y, Sun W (2012). "Bivariate random effects models for meta-analysis of comparative studies with binary outcomes: methods for the absolute risk difference and relative risk." \emph{Statistical Methods in Medical Research}, \bold{21}(6), 621--633. <\doi{10.1177/0962280210393712}>

Hamza TH, van Houwelingen HC, Stijnen T (2008). "The binomial distribution of meta-analysis was preferred to model within-study variability." \emph{Journal of Clinical Epidemiology}, \bold{61}(1), 41--51. <\doi{10.1016/j.jclinepi.2007.03.016}>

Lin L, Chu H (2020). "Meta-analysis of proportions using generalized linear mixed models." \emph{Epidemiology}, \bold{31}(5), 713--717. <\doi{10.1097/ede.0000000000001232}>

Stijnen T, Hamza TH, Ozdemir P (2010). "Random effects meta-analysis of event outcome in the framework of the generalized linear mixed model with applications in sparse data." \emph{Statistics in Medicine}, \bold{29}(29), 3046--3067. <\doi{10.1002/sim.4040}>

Zeger SL, Liang K-Y, Albert PS (1988). "Models for longitudinal data: a generalized estimating equation approach." \emph{Biometrics}, \bold{44}(4), 1049--1060. <\doi{10.2307/2531734}>
}
%%\author{
%%  ~~who you are~~
%%}
\note{
This function implements the GLMM for the meta-analysis of proportions via the function \code{\link[lme4]{glmer}} in the package \pkg{lme4}. It is possible that the algorithm of the GLMM estimation may not converge for some bootstrapped meta-analyses when \code{pop.avg} = \code{TRUE}, and the function \code{\link[lme4]{glmer}} may report warnings or errors about the convergence issue. The bootstrap iterations are continued until \code{b.iter} replicates without any warnings or errors are obtained; those replicates with any warnings or errors are discarded.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maprop.twostep}}
}
\examples{
# chorioamnionitis data
data("dat.chor")
# GLMM with the logit link with only 10 bootstrap iterations
out.chor.glmm.logit <- maprop.glmm(e, n, data = dat.chor,
  link = "logit", b.iter = 10, seed = 1234)
out.chor.glmm.logit
# not calculating the population-averaged (marginal) proportion,
#  without bootstrap resampling
out.chor.glmm.logit <- maprop.glmm(e, n, data = dat.chor,
  link = "logit", pop.avg = FALSE)
out.chor.glmm.logit
\donttest{
# increases the number of bootstrap iterations to 1000,
#  taking longer time
out.chor.glmm.logit <- maprop.glmm(e, n, data = dat.chor,
  link = "logit", b.iter = 1000, seed = 1234)
out.chor.glmm.logit

# GLMM with the log link
out.chor.glmm.log <- maprop.glmm(e, n, data = dat.chor,
  link = "log", b.iter = 10, seed = 1234)
out.chor.glmm.log
# GLMM with the probit link
out.chor.glmm.probit <- maprop.glmm(e, n, data = dat.chor,
  link = "probit", b.iter = 10, seed = 1234)
out.chor.glmm.probit
# GLMM with the cauchit link
out.chor.glmm.cauchit <- maprop.glmm(e, n, data = dat.chor,
  link = "cauchit", b.iter = 10, seed = 1234)
out.chor.glmm.cauchit
# GLMM with the cloglog link
out.chor.glmm.cloglog <- maprop.glmm(e, n, data = dat.chor,
  link = "cloglog", b.iter = 10, seed = 1234)
out.chor.glmm.cloglog


# depression data
data("dat.beck17")
out.beck17.glmm.log <- maprop.glmm(e, n, data = dat.beck17,
  link = "log", b.iter = 10, seed = 1234)
out.beck17.glmm.log
out.beck17.glmm.logit <- maprop.glmm(e, n, data = dat.beck17,
  link = "logit", b.iter = 10, seed = 1234)
out.beck17.glmm.logit
out.beck17.glmm.probit <- maprop.glmm(e, n, data = dat.beck17,
  link = "probit", b.iter = 10, seed = 1234)
out.beck17.glmm.probit
out.beck17.glmm.cauchit <- maprop.glmm(e, n, data = dat.beck17,
  link = "cauchit", b.iter = 10, seed = 1234)
out.beck17.glmm.cauchit
out.beck17.glmm.cloglog<- maprop.glmm(e, n, data = dat.beck17,
  link = "cloglog", b.iter = 10, seed = 1234)
out.beck17.glmm.cloglog
}
}
\keyword{generalized linear mixed model}
\keyword{proportion}
