\name{metapb}
\alias{metapb}

\title{
Detect and Quantify Publication Bias
}
\description{
Performs the regression test and calculates skewness for detecting and quantifying publication bias.
}
\usage{
metapb(y, s2, model)
}

\arguments{
  \item{y}{
  a numeric vector indicating the observed effect sizes in the collected studies; they are assumed to be normally distributed.
}
  \item{s2}{
  a numeric vector indicating the within-study variances.
}
  \item{model}{
  a characher string specifying the fixed-effect (\code{"FE"}) or random-effects (\code{"RE"}) model. If not specified, this function uses the \eqn{Q} statistic to test for heterogeneity: if the \eqn{p}-value is smaller than 0.05, \code{model} is set to \code{"RE"}; otherwise, \code{model = "FE"}.
}
}
\details{
This function derives the measures of publication bias considered in Lin and Chu (2018).
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
  \item{reg.pval}{the p-value of the regression intercept.}
  \item{skewness}{the estimate of the skewness for quantifying publication bias.}
  \item{skewness.ci}{the 95\% CI of the skewness.}
  \item{skewness.pval}{the p-value of the skewness.}
  \item{combined.pval}{the p-value of the combined test that incorporates the regression intercept and the skewness.}
}
\references{
Egger M, Smith GD, Schneider M, and Minder C (1997). "Bias in meta-analysis detected by a simple, graphical test." \emph{BMJ}, \bold{315}(7109), 629--34. <doi:10.1136/bmj.315.7109.629>

Lin L, Chu H (2018). "Quantifying publication bias in meta-analysis." \emph{Biometrics}, \bold{74}(3), 785--794. <doi:10.1111/biom.12817>
}

%%\note{
%%  ~~further notes~~
%%}

\examples{
data("dat.slf")
attach(dat.slf)
metapb(y, s2)
detach(dat.slf)

data("dat.ha")
attach(dat.ha)
metapb(y, s2)
detach(dat.ha)

data("dat.lcj")
attach(dat.lcj)
metapb(y, s2)
detach(dat.lcj)
}