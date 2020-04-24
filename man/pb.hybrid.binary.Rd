\name{pb.hybrid.binary}
\alias{pb.hybrid.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hybrid Test for Publication Bias in Meta-Analysis With Binary Outcomes
}
\description{
Performs the hybrid test for publication bias introduced in Lin (2020), which synthesizes results from multiple popular publication bias tests, in a meta-analysis with binary outcomes.
}
\usage{
pb.hybrid.binary(n00, n01, n10, n11, data, methods,
                 iter.resam = 1000, theo.pval = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n00}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the counts of non-events in treatment group 0 in the collected studies.
}
  \item{n01}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the counts of events in treatment group 0 in the collected studies.
}
  \item{n10}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the counts of non-events in treatment group 1 in the collected studies.
}
  \item{n11}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the counts of events in treatment group 1 in the collected studies.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{n00}, \code{n01}, \code{n10}, and \code{n11}, should be specified as their corresponding column names in \code{data}.
}
  \item{methods}{
  a vector of character strings indicating the publication bias tests to be included in the hybrid test. They can be a subset of \code{"rank"} (Begg's rank test; see Begg and Mazumdar, 1994), \code{"reg"} (Egger's regression test under the fixed-effect setting; see Egger et al., 1997), \code{"reg.het"} (Egger's regression test accounting for additive heterogeneity), \code{"skew"} (the skewness-based test under the fixed-effect setting; see Lin and Chu, 2018), \code{"skew.het"} (the skewness-based test accounting for additive heterogeneity), \code{"inv.sqrt.n"} (the regression test based on sample sizes; see Tang and Liu, 2000), \code{"trimfill"} (the trim-and-fill method; see Duval and Tweedie, 2000), \code{"n"} (the regressoin test with sample sizes as the predictor; see Macaskill et al., 2001), \code{"inv.n"} (the regressoin test with the inverse of sample sizes as the predictor; see Peters et al., 2006), \code{"as.rank"} (the rank test based on the arcsine-transformed effect sizes; see Rucker et al., 2008), \code{"as.reg"} (the regression test based on the arcsine-transformed effect sizes under the fixed-effect setting), \code{"as.reg.het"} (the regression test based on the arcsine-transformed effect sizes accounting for additive heterogeneity), \code{"smoothed"} (the regression test based on the smoothed sample variances under the fixed-effect setting; see Jin et al., 2014), \code{"smoothed.het"} (the regression test based on the smoothed sample variances accounting for additive heterogeneity), \code{"score"} (the regression test based on the score function; see Harbord et al., 2006), and \code{"count"} (the test based on the hypergeometric distributions of event counts, designed for sparse data; see Schwarzer et al., 2007). The default is to include all aforementioned tests.
}
  \item{iter.resam}{
  a positive integer indicating the number of resampling iterations for calculating the p-value of the hybrid test.
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

Harbord RM, Egger M, Sterne JAC (2006). "A modified test for small-study effects in meta-analyses of controlled trials with binary endpoints." \emph{Statistics in Medicine}, \bold{25}(20), 3443--3457. <\doi{10.1002/sim.2380}>

Jin Z-C, Wu C, Zhou X-H, He J (2014). "A modified regression method to test publication bias in meta-analyses with binary outcomes." \emph{BMC Medical Research Methodology}, \bold{14}, 132. <\doi{10.1186/1471-2288-14-132}>

Lin L (2020). "Hybrid test for publication bias in meta-analysis." \emph{Statistical Methods in Medical Research}, in press. <\doi{10.1177/0962280220910172}>

Lin L, Chu H (2018). "Quantifying publication bias in meta-analysis." \emph{Biometrics}, \bold{74}(3), 785--794. <\doi{10.1111/biom.12817}>

Macaskill P, Walter SD, Irwig L (2001). "A comparison of methods to detect publication bias in meta-analysis." \emph{Statistics in Medicine}, \bold{20}(4), 641--654. <\doi{10.1002/sim.698}>

Peters JL, Sutton AJ, Jones DR, Abrams KR, Rushton L (2006). "Comparison of two methods to detect publication bias in meta-analysis." \emph{JAMA}, \bold{295}(6), 676--680. <\doi{10.1001/jama.295.6.676}>

Rucker G, Schwarzer G, Carpenter J (2008). "Arcsine test for publication bias in meta-analyses with binary outcomes." \emph{Statistics in Medicine}, \bold{27}(5), 746--763. <\doi{10.1002/sim.2971}>

Schwarzer G, Antes G, Schumacher M (2007). "A test for publication bias in meta-analysis with sparse binary data." \emph{Statistics in Medicine}, \bold{26}(4), 721--733. <\doi{10.1002/sim.2588}>

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
\code{\link{pb.hybrid.generic}}
}
\examples{
## meta-analysis of (log) odds ratios
data("dat.whiting")
# based on only 10 resampling iterations
set.seed(1234)
out.whiting <- pb.hybrid.binary(n00 = n00, n01 = n01,
  n10 = n10, n11 = n11, data = dat.whiting, iter.resam = 10)
out.whiting
\donttest{
# increases the number of resampling iterations to 10000,
#  taking longer time
set.seed(1234)
out.whiting <- pb.hybrid.binary(n00 = n00, n01 = n01,
  n10 = n10, n11 = n11, data = dat.whiting, iter.resam = 10000)
out.whiting
}
}
\keyword{publication bias}
