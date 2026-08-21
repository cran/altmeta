\name{pb.timelag}
\alias{pb.timelag}
\title{
Flexible Nonparametric Assessment of Time-Lag Bias in Meta-Analysis
}
\description{
Performs permutation-calibrated fixed-gamma tests and an adaptive hybrid test for assessing time-lag bias in meta-analysis introduced in Xing et al.\sspace(2026).
}
\usage{
pb.timelag(y, v, time, B = 5000, tau2 = NULL)
}
\arguments{
  \item{y}{a numeric vector specifying the observed effect sizes in the collected studies.}
  \item{v}{a numeric vector specifying the corresponding within-study variances.}
  \item{time}{a numeric vector specifying the publication times of the studies.}
  \item{B}{a positive integer specifying the number of permutation iterations. The default is 5000.}
  \item{tau2}{an optional non-negative value specifying the between-study variance. If \code{NULL}, it is estimated using the restricted maximum-likelihood method.}
}
\details{
The studies are first ordered by publication time. For each position in the ordered sequence, the function constructs a standardized cumulative contrast between the corresponding study and all later studies. The resulting contrasts are combined using powers 1 through 8 and the maximum statistic.

The null distributions of the fixed-gamma statistics are obtained by jointly permuting each study's effect size and within-study variance with respect to publication order. The hybrid test uses the minimum fixed-gamma permutation p-value and an additional permutation calibration to account for the adaptive selection across candidate statistics.

When multiple studies have the same value of \code{time}, their ordering is determined by their original order in the supplied vectors.
}
\value{
A list containing the following components:
\describe{
  \item{\code{p.value}}{the permutation p-values for \code{T1} through \code{T8}, \code{TInf}, and the hybrid test.}
  \item{\code{statistic}}{the observed values of \code{T1} through \code{T8} and \code{TInf}.}
  \item{\code{tau2}}{the supplied or estimated between-study variance.}
  \item{\code{B}}{the number of permutation iterations.}
}
}
\references{
Xing X, Yu Z, Xiao M, Tong J, Lin L (2026). "Flexible nonparametric assessment of time-lag bias in meta-analysis." \emph{Biostatistics}, in press.
}
\author{
Zhiyuan Yu, Xing Xing, Lifeng Lin 
}
\seealso{
\code{\link{pb.bayesian.binary}}, \code{\link{pb.hybrid.binary}}, \code{\link{pb.hybrid.generic}}
}
\examples{
data("dat.sherrington")

set.seed(100)
out <- pb.timelag(
  y = dat.sherrington$yi,
  v = dat.sherrington$vi,
  time = dat.sherrington$year,
  B = 100
)
out$p.value
}
\keyword{publication bias/small-study effects}
