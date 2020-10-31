\name{pb.hybrid.generic}
\alias{pb.hybrid.generic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hybrid Test for Publication Bias/Small-Study Effects in Meta-Analysis With Generic Outcomes
}
\description{
Performs the hybrid test for publication bias/small-study effects introduced in Lin (2020), which synthesizes results from multiple popular publication bias tests, in a meta-analysis with generic outcomes.
}
\usage{
pb.hybrid.generic(y, s2, n, data, methods,
                  iter.resam = 1000, theo.pval = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the observed effect sizes in the collected studies.
}
  \item{s2}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the within-study variances.
}
  \item{n}{
  an optional numeric vector or the corresponding column name in the argument \code{data}, specifying the study-specific total sample sizes. This argument is required if the sample-size-based test (\code{"inv.sqrt.n"}) is included in \code{method}.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{y}, \code{s2}, and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{methods}{
  a vector of character strings specifying the publication bias tests to be included in the hybrid test. They can be a subset of \code{"rank"} (Begg's rank test; see Begg and Mazumdar, 1994), \code{"reg"} (Egger's regression test under the fixed-effect setting; see Egger et al., 1997), \code{"reg.het"} (Egger's regression test accounting for additive heterogeneity), \code{"skew"} (the skewness-based test under the fixed-effect setting; see Lin and Chu, 2018), \code{"skew.het"} (the skewness-based test accounting for additive heterogeneity), \code{"inv.sqrt.n"} (the regression test based on sample sizes; see Tang and Liu, 2000), and \code{"trimfill"} (the trim-and-fill method; see Duval and Tweedie, 2000). The default is to include all aforementioned tests.
}
  \item{iter.resam}{
  a positive integer specifying the number of resampling iterations for calculating the p-value of the hybrid test.
}
  \item{theo.pval}{
  a logical value indicating whether additionally calculating the p-values of the tests specified in \code{methods} based on the test statistics' theoretical null distributions. Regardless of this argument, the resampling-based p-values are always produced by this function for the tests specified in \code{methods}.
}
}
\details{
The hybrid test statistic is defined as the minimum p-value among the publication bias tests considered in the set specified by the argument \code{methods}. Note that the minimum p-value is no longer a genuine p-value because it cannot control the type I error rate. Its p-value needs to be calculated via the resampling approach. See more details in Lin (2020).
}
\value{
This function returns a list containing p-values of the publication bias tests specified in \code{methods} as well as the hybrid test. Each element's name in this list has the format of \code{pval.x}, where \code{x} stands for the character string corresponding to a certain publication bias test, such as \code{rank}, \code{reg}, \code{skew}, etc. The hybrid test's p-value has the name \code{pval.hybrid}. If \code{theo.pval} = \code{TRUE}, additional elements of p-values of the tests in \code{methods} based on theorectical null distributions are included in the produced list; their names have the format of \code{pval.x.theo}. Another p-value of the hybrid test is also produced based on them; its corresponding element has the name \code{pval.hybrid.theo}.
}
\references{
Begg CB, Mazumdar M (1994). "Operating characteristics of a rank correlation test for publication bias." \emph{Biometrics}, \bold{50}(4), 1088--1101. <\doi{10.2307/2533446}>

Duval S, Tweedie R (2000). "A nonparametric `trim and fill' method of accounting for publication bias in meta-analysis." \emph{Journal of the American Statistical Association}, \bold{95}(449), 89--98. <\doi{10.1080/01621459.2000.10473905}>

Egger M, Davey Smith G, Schneider M, Minder C (1997). "Bias in meta-analysis detected by a simple, graphical test." \emph{BMJ}, \bold{315}(7109), 629--634. <\doi{10.1136/bmj.315.7109.629}>

Lin L (2020). "Hybrid test for publication bias in meta-analysis." \emph{Statistical Methods in Medical Research}, \bold{29}(10), 2881--2899. <\doi{10.1177/0962280220910172}>

Lin L, Chu H (2018). "Quantifying publication bias in meta-analysis." \emph{Biometrics}, \bold{74}(3), 785--794. <\doi{10.1111/biom.12817}>

Tang J-L, Liu JLY (2000). "Misleading funnel plot for detection of bias in meta-analysis." \emph{Journal of Clinical Epidemiology}, \bold{53}(5), 477--484. <\doi{10.1016/S0895-4356(99)00204-8}>

Thompson SG, Sharp SJ (1999). "Explaining heterogeneity in meta-analysis: a comparison of methods." \emph{Statistics in Medicine}, \bold{18}(20), 2693--2708. <\doi{10.1002/(SICI)1097-0258(19991030)18:20<2693::AID-SIM235>3.0.CO;2-V}>
}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pb.bayesian.binary}}, \code{\link{pb.hybrid.binary}}
}
\examples{
## meta-analysis of mean differences
data("dat.plourde")
# based on only 10 resampling iterations
set.seed(1234)
out.plourde <- pb.hybrid.generic(y = y, s2 = s2, n = n,
  data = dat.plourde, iter.resam = 10)
out.plourde
# only produces resampling-based p-values
set.seed(1234)
pb.hybrid.generic(y = y, s2 = s2, n = n,
  data = dat.plourde, iter.resam = 10, theo.pval = FALSE)
# increases the number of resampling iterations to 10000,
#  taking longer time

## meta-analysis of standardized mean differences
data("dat.paige")
# based on only 10 resampling iterations
set.seed(1234)
out.paige <- pb.hybrid.generic(y = y, s2 = s2, n = n,
  data = dat.paige, iter.resam = 10)
out.paige
# increases the number of resampling iterations to 10000,
#  taking longer time
}
\keyword{publication bias/small-study effects}
