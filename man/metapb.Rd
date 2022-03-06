\name{metapb}
\alias{metapb}

\title{
Detecting and Quantifying Publication Bias/Small-Study Effects
}
\description{
Performs the regression test and calculates skewness for detecting and quantifying publication bias/small-study effects.
}
\usage{
metapb(y, s2, data, model = "RE", n.resam = 1000)
}

\arguments{
  \item{y}{
  a numeric vector specifying the observed effect sizes in the collected studies; they are assumed to be normally distributed.
}
  \item{s2}{
  a numeric vector specifying the within-study variances.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{y} and \code{s2}, should be specified as their corresponding column names in \code{data}.
}
  \item{model}{
  a characher string specifying the fixed-effect (\code{"FE"}) or random-effects (\code{"RE"}, the default) model. If not specified, this function uses the \eqn{Q} statistic to test for heterogeneity: if the p-value is smaller than 0.05, \code{model} is set to \code{"RE"}; otherwise, \code{model} = \code{"FE"}.
}
  \item{n.resam}{
  a positive integer specifying the number of resampling iterations.
}
}
\details{
This function derives the measures of publication bias introduced in Lin and Chu (2018).
}
\value{
This function returns a list containing measures of publication bias, their 95\% confidence intervals, and p-values. Specifically, the components include:
  \item{n}{the number of studies in the meta-analysis.}
  \item{p.Q}{the p-value of the \eqn{Q}-test for heterogeneity.}
  \item{I2}{the \eqn{I^2} statistic for quantifying heterogeneity.}
  \item{tau2}{the DerSimonian--Laird estimate of the between-study variance.}
  \item{model}{the model setting (\code{"FE"} or \code{"RE"}).}
  \item{std.dev}{the standardized deviates of the studies.}
  \item{reg.int}{the estimate of the regression intercept for quantifying publication bias.}
  \item{reg.int.ci}{the 95\% CI of the regression intercept.}
  \item{reg.int.ci.resam}{the 95\% CI of the regression intercept based on the resampling method.}
  \item{reg.pval}{the p-value of the regression intercept.}
  \item{reg.pval}{the p-value of the regression intercept based on the resampling method.}
  \item{skewness}{the estimate of the skewness for quantifying publication bias.}
  \item{skewness.ci}{the 95\% CI of the skewness.}
  \item{skewness.ci.resam}{the 95\% CI of the skewness based on the resampling method.}
  \item{skewness.pval}{the p-value of the skewness.}
  \item{skewness.pval.resam}{the p-value of the skewness based on the resampling method.}
  \item{combined.pval}{the p-value of the combined test that incorporates the regression intercept and the skewness.}
  \item{combined.pval.resam}{the p-value of the combined test that incorporates the regression intercept and the skewness based on the resampling method.}
}
\references{
Egger M, Davey Smith G, Schneider M, Minder C (1997). "Bias in meta-analysis detected by a simple, graphical test." \emph{BMJ}, \bold{315}(7109), 629--634. <\doi{10.1136/bmj.315.7109.629}>

Lin L, Chu H (2018). "Quantifying publication bias in meta-analysis." \emph{Biometrics}, \bold{74}(3), 785--794. <\doi{10.1111/biom.12817}>
}

%%\note{
%%  ~~further notes~~
%%}

\examples{
data("dat.slf")
set.seed(1234)
metapb(y, s2, dat.slf)

data("dat.ha")
set.seed(1234)
metapb(y, s2, dat.ha)

data("dat.lcj")
set.seed(1234)
metapb(y, s2, dat.lcj)
}
\keyword{publication bias/small-study effects}