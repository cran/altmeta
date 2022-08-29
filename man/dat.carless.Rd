\name{dat.carless}
\alias{dat.carless}
\docType{data}
\title{
A Meta-Analysis on the Efficacy of Platelet-Rich-Plasmapheresis in Reducing Peri-Operative Allogeneic Red Blood Cell Transfusion in Cardiac Surgery
}
\description{
This meta-analysis serves as an example to illustrate the usage of the function \code{\link{meta.pen}}.
}
\usage{data("dat.ha")}
\format{
  A data frame containing 20 studies.
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
Carless PA, Rubens FD, Anthony DM, O'Connell D, Henry DA (2011). "Platelet-rich-plasmapheresis for minimising peri-operative allogeneic blood transfusion." \emph{Cochrane Database of Systematic Reviews}, \bold{3}. Art.{\sspace}No.: CD004172. <\doi{10.1002/14651858.CD004172.pub2}>
}
\keyword{dataset}