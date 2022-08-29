\name{dat.bohren}
\alias{dat.bohren}
\docType{data}
\title{
A Meta-Analysis on the Effects of Continuous, One-To-One Intrapartum Support on Spontaneous Vaginal Births, Compared With Usual Care
}
\description{
This meta-analysis serves as an example to illustrate the usage of the function \code{\link{meta.pen}}.
}
\usage{data("dat.ha")}
\format{
  A data frame containing 21 studies.
  \describe{
    \item{\code{n1}}{the sample size in the treatment group in each study.}
    \item{\code{n2}}{the sample size in the control group in each study.}
    \item{\code{r1}}{the event count in the treatment group in each study.}
    \item{\code{r2}}{the event count in the control group in each study.}
    \item{\code{y}}{the observed effect size (log odds ratio) for each study in the meta-analysis.}
    \item{\code{s2}}{the within-study variance for each study.}
  }
}
\source{
Bohren MA, Hofmeyr GJ, Sakala C, Fukuzawa RK, Cuthbert A (2017). "Continuous support for women during childbirth." \emph{Cochrane Database of Systematic Reviews}, \bold{7}. Art.{\sspace}No.: CD003766. <\doi{10.1002/14651858.CD003766.pub6}>
}
\keyword{dataset}