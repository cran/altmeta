\name{dat.bjelakovic}
\alias{dat.bjelakovic}
\docType{data}
\title{
A Meta-Analysis on the Beneficial and Harmful Effects of Vitamin D Supplementation for the Prevention of Mortality in Healthy Adults and Adults in a Stable Phase of Disease
}
\description{
This meta-analysis serves as an example to illustrate the usage of the function \code{\link{meta.pen}}.
}
\usage{data("dat.ha")}
\format{
  A data frame containing 53 studies.
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
Bjelakovic G, Gluud LL, Nikolova D, Whitfield K, Wetterslev J, Simonetti RG, Bjelakovic M, Gluud C (2014). "Vitamin D supplementation for prevention of mortality in adults." \emph{Cochrane Database of Systematic Reviews}, \bold{7}. Art.{\sspace}No.: CD007470. <\doi{10.1002/14651858.CD007470.pub3}>
}
\keyword{dataset}