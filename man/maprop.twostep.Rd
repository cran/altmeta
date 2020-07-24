\name{maprop.twostep}
\alias{maprop.twostep}
\title{
Meta-Analysis of Proportions Using Two-Step Methods
}
\description{
Performs a meta-analysis of proportions using conventional two-step methods with various data transformations.
}
\usage{
maprop.twostep(e, n, data, link = "logit", method = "ML", alpha = 0.05,
               pop.avg = TRUE, int.approx = 10000, b.iter = 1000,
               seed = 1234)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{
  a numeric vector indicating the event counts in the collected studies.
}
  \item{n}{
  a numeric vector indicating the sample sizes in the collected studies.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{e} and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{link}{
  a character string specifying the data transformation for each study's proportion used in the two-step method, which can be one of \code{"log"} (log transformation), \code{"logit"} (logit transformation, the default), \code{"arcsine"} (arcsine transformation), and \code{"double.arcsine"} (Freeman--Tukey double-arcsine transformation).
}
  \item{method}{
  a character string specifying the method to perform the meta-analysis, which is passed to the argument \code{method} in the function \code{\link[metafor]{rma.uni}} in the package \pkg{metafor}. It can be one of \code{"ML"} (maximum likelihood, the default), \code{"REML"} (restricted maximum likelihood), and many other options; see more details in the manual of \pkg{metafor}. The default is set to \code{"ML"} for consistency with the function \code{\link{maprop.glmm}}, where generalized linear mixed models are often estimated via the maximum likelihood approach. For the two-step method, users might also use \code{"REML"} because the restricted maximum likelihood estimation may have superior performance in many cases.
}
  \item{alpha}{
  a numeric value indicating the statistical significance level.
}
  \item{pop.avg}{
  a logical value indicating whether the population-averaged proportion and its confidence interval are to be produced. This quantity is the marginal mean of study-specific proportions, while the commonly-reported overall proportion usually represents the median (or interpreted as a conditional measure); see more details about this quantity in Section 13.2.3 in Agresti (2013), Chu et al.{\sspace}(2012), Lin and Chu (2020), and Zeger et al.{\sspace}(1988). If \code{pop.avg} = \code{TRUE} (the default), the bootstrap resampling is used to produce the confidence interval of the population-averaged proportion; the confidence interval of the commonly-reported median proportion will be also produced, in addition to its conventional confidence interval (by back-transforming the Wald-type confidence interval derived on the scale specified by \code{link}).
}
  \item{int.approx}{
  an integer indicating the number of independent standard normal samples for numerically approximating the integration involved in the calculation of the population-averaged proportion; see details in Lin and Chu (2020). It is only used when \code{pop.avg} = \code{TRUE}. For the commonly-used data transformations available for \code{link}, the population-averaged proportion does not have a closed form.
}
  \item{b.iter}{
  an integer indicating the number of bootstrap iterations; it is only used when \code{pop.avg} = \code{TRUE}.
}
  \item{seed}{
  an integer for specifying the seed of the random number generation for reproducibility during the bootstrap resampling (and numerical approximation for the population-averaged proportion); it is only used when \code{pop.avg} = \code{TRUE}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function returns a list containing the point and interval estimates of the overall proportion. Specifically, \code{prop.c.est} is the commonly-reported median (or conditional) proportion, and \code{prop.c.ci} is its confidence interval. If \code{pop.avg} = \code{TRUE}, the following additional elements will be also in the produced list: \code{prop.c.ci.b} is the bootstrap confidence interval of the commonly-reported median (conditional) proportion, \code{prop.m.est} is the point estimate of the population-averaged (marginal) proportion, \code{prop.m.ci.b} is the bootstrap confidence interval of the population-averaged (marginal) proportion, and \code{b.w.e} is a vector of two numeric values, indicating the counts of warnings and errors occurred during the bootstrap iterations. Moreover, if the Freeman--Tukey double-arcsine transformation (\code{link} = \code{"double.arcsine"}) is used, the back-transformation will be implemented at four values as the overall sample size: the harmonic, geometric, and arithmetic means of the study-specific sample sizes, and the inverse of the synthesized result's variance. See details in Barendregt et al.{\sspace}(2013) and Schwarzer et al.{\sspace}(2019).
}
\references{
Agresti A (2013). \emph{Categorical Data Analysis}. Third edition. John Wiley & Sons, Hoboken, NJ.

Barendregt JJ, Doi SA, Lee YY, Norman RE, Vos T (2013). "Meta-analysis of prevalence." \emph{Journal of Epidemiology and Community Health}, \bold{67}(11), 974--978. <\doi{10.1136/jech-2013-203104}>

Freeman MF, Tukey JW (1950). "Transformations related to the angular and the square root." \emph{The Annals of Mathematical Statistics}, \bold{21}(4), 607--611. <\doi{10.1214/aoms/1177729756}>

Lin L, Chu H (2020). "Meta-analysis of proportions using generalized linear mixed models." \emph{Epidemiology}, in press. <\doi{10.1097/ede.0000000000001232}>

Miller JJ (1978). "The inverse of the Freeman--Tukey double arcsine transformation." \emph{The American Statistician}, \bold{32}(4), 138. <\doi{10.1080/00031305.1978.10479283}>

Schwarzer G, Chemaitelly H, Abu-Raddad LJ, Rucker G (2019). "Seriously misleading results using inverse of Freeman-Tukey double arcsine transformation in meta-analysis of single proportions." \emph{Research Synthesis Methods}, \bold{10}(3), 476--483. <\doi{10.1002/jrsm.1348}>

Viechtbauer W (2010). "Conducting meta-analyses in R with the metafor package." \emph{Journal of Statistical Software}, \bold{36}, 3. <\doi{10.18637/jss.v036.i03}>

Zeger SL, Liang K-Y, Albert PS (1988). "Models for longitudinal data: a generalized estimating equation approach." \emph{Biometrics}, \bold{44}(4), 1049--1060. <\doi{10.2307/2531734}>
}
%%\author{
%%  ~~who you are~~
%%}
\note{
This function implements the two-step method for the meta-analysis of proportions via the \code{\link[metafor]{rma.uni}} function in the package \pkg{metafor}. It is possible that the algorithm of the maximum likelihood or restricted maximum likelihood estimation may not converge for some bootstrapped meta-analyses when \code{pop.avg} = \code{TRUE}, and the \code{\link[metafor]{rma.uni}} function may report warnings or errors about the convergence issue. The bootstrap iterations are continued until \code{b.iter} replicates without any warnings or errors are obtained; those replicates with any warnings or errors are discarded.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maprop.glmm}}
}
\examples{
# chorioamnionitis data
data("dat.chor")
# two-step method with the logit transformation
out.chor.twostep.logit <- maprop.twostep(e, n, data = dat.chor,
  link = "logit", b.iter = 10, seed = 1234)
out.chor.twostep.logit
# not calculating the population-averaged (marginal) proportion,
#  without bootstrap resampling
out.chor.twostep.logit <- maprop.twostep(e, n, data = dat.chor,
  link = "logit", pop.avg = FALSE)
out.chor.twostep.logit
\donttest{
# increases the number of bootstrap iterations to 1000,
#  taking longer time
out.chor.twostep.logit <- maprop.twostep(e, n, data = dat.chor,
  link = "logit", b.iter = 1000, seed = 1234)
out.chor.twostep.logit

# two-step method with the log transformation
out.chor.twostep.log <- maprop.twostep(e, n, data = dat.chor,
  link = "log", b.iter = 10, seed = 1234)
out.chor.twostep.log
# two-step method with the arcsine transformation
out.chor.twostep.arcsine <- maprop.twostep(e, n, data = dat.chor,
  link = "arcsine", b.iter = 10, seed = 1234)
out.chor.twostep.arcsine
# two-step method with the Freeman--Tukey double-arcsine transformation
out.chor.twostep.double.arcsine <- maprop.twostep(e, n, data = dat.chor,
  link = "double.arcsine", b.iter = 10, seed = 1234)
out.chor.twostep.double.arcsine


# depression data
data("dat.beck17")
out.beck17.twostep.log <- maprop.twostep(e, n, data = dat.beck17,
  link = "log", b.iter = 10, seed = 1234)
out.beck17.twostep.log
out.beck17.twostep.logit <- maprop.twostep(e, n, data = dat.beck17,
  link = "logit", b.iter = 10, seed = 1234)
out.beck17.twostep.logit
out.beck17.twostep.arcsine <- maprop.twostep(e, n, data = dat.beck17,
  link = "arcsine", b.iter = 10, seed = 1234)
out.beck17.twostep.arcsine
out.beck17.twostep.double.arcsine <- maprop.twostep(e, n, data = dat.beck17,
  link = "double.arcsine", b.iter = 10, seed = 1234)
out.beck17.twostep.double.arcsine
}
}
\keyword{proportion}
