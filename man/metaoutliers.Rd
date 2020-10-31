\name{metaoutliers}
\alias{metaoutliers}

\title{
Outlier Detection in Meta-Analysis
}
\description{
Calculates the standardized residual for each study in meta-analysis using the methods desribed in Chapter 12 in Hedges and Olkin (1985) and Viechtbauer and Cheung (2010). A study is considered as an outlier if its standardized residual is greater than 3 in absolute magnitude.
}
\usage{
metaoutliers(y, s2, model)
}

\arguments{
  \item{y}{
  a numeric vector specifying the observed effect sizes in the collected studies; they are assumed to be normally distributed.
}
  \item{s2}{
  a numeric vector specifying the within-study variances.
}
  \item{model}{
  a character string specified as either \code{"FE"} or \code{"RE"}. If \code{model} = \code{"FE"}, this function uses the outlier detection procedure for the fixed-effect meta-analysis desribed in Chapter 12 in Hedges and Olkin (1985); If \code{model} = \code{"RE"}, the procedure for the random-effects meta-analysis desribed in Viechtbauer and Cheung (2010) is used. See Details for the two approaches. If the argument \code{model} is not specified, this function sets \code{model} = \code{"FE"} if \eqn{I_r^2 < 30\%} and sets \code{model} = \code{"RE"} if \eqn{I_r^2 \geq 30\%}.
}
}
\details{
Suppose that a meta-analysis collects \eqn{n} studies. The observed effect size in study \eqn{i} is \eqn{y_i} and its within-study variance is \eqn{s^{2}_{i}}. Also, the inverse-variance weight is \eqn{w_i = 1 / s^{2}_{i}}.

Chapter 12 in Hedges and Olkin (1985) describes the outlier detection procedure for the fixed-effect meta-analysis (\code{model} = \code{"FE"}). Using the studies except study \eqn{i}, the pooled estimate of the overall effect size is \eqn{\bar{\mu}_{(-i)} = \sum_{j \neq i} w_j y_j / \sum_{j \neq i} w_j}. The residual of study \eqn{i} is \eqn{e_{i} = y_i - \bar{\mu}_{(-i)}}. The variance of \eqn{e_{i}} is \eqn{v_{i} = s_{i}^{2} + (\sum_{j \neq i} w_{j})^{-1}}, so the standardized residual of study \eqn{i} is \eqn{\epsilon_{i} = e_{i} / \sqrt{v_{i}}}.

Viechtbauer and Cheung (2010) describes the outlier detection procedure for the random-effects meta-analysis (\code{model} = \code{"RE"}). Using the studies except study \eqn{i}, let the method-of-moments estimate of the between-study variance be \eqn{\hat{\tau}_{(-i)}^{2}}. The pooled estimate of the overall effect size is \eqn{\bar{\mu}_{(-i)} = \sum_{j \neq i} \tilde{w}_{(-i)j} y_j / \sum_{j \neq i} \tilde{w}_{(-i)j}}, where \eqn{\tilde{w}_{(-i)j} = 1/(s_{j}^{2} + \hat{\tau}_{(-i)}^{2})}. The residual of study \eqn{i} is \eqn{e_{i} = y_i - \bar{\mu}_{(-i)}}, and its variance is \eqn{v_{i} = s_{i}^2 + \hat{\tau}_{(-i)}^{2} + (\sum_{j \neq i} \tilde{w}_{(-i)j})^{-1}}. Then, the standardized residual of study \eqn{i} is \eqn{\epsilon_{i} = e_{i} / \sqrt{v_{i}}}. 
}
\value{
This functions returns a list which contains standardized residuals and identified outliers. A study is considered as an outlier if its standardized residual is greater than 3 in absolute magnitude.
}
\references{
Hedges LV, Olkin I (1985). \emph{Statistical Method for Meta-Analysis}. Academic Press, Orlando, FL.

Viechtbauer W, Cheung MWL (2010). "Outlier and influence diagnostics for meta-analysis." \emph{Research Synthesis Methods}, \bold{1}(2), 112--125. <\doi{10.1002/jrsm.11}>
}

\examples{
data("dat.aex")
attach(dat.aex)
metaoutliers(y, s2, model = "FE")
metaoutliers(y, s2, model = "RE")
detach(dat.aex)

data("dat.hipfrac")
attach(dat.hipfrac)
metaoutliers(y, s2)
detach(dat.hipfrac)
}
\keyword{heterogeneity}