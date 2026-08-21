\name{dat.sherrington}
\alias{dat.sherrington}
\docType{data}
\title{
A Meta-Analysis on the Effects of Exercise Interventions for Preventing Falls in Older People
}
\description{
This meta-analysis serves as an example to illustrate the usage of the function \code{\link{pb.timelag}}.
}
\usage{data("dat.sherrington")}
\format{
  A data frame containing 19 studies.
  \describe{
    \item{\code{study}}{the study label for each study in the meta-analysis.}
    \item{\code{year}}{the publication year for each study.}
    \item{\code{yi}}{the observed effect size (log rate ratio) for each study in the meta-analysis.}
    \item{\code{vi}}{the within-study variance for each study.}
    \item{\code{n1}}{the sample size in the exercise intervention group.}
    \item{\code{n2}}{the sample size in the control group.}
  }
}
\source{
Sherrington C, Fairhall NJ, Wallbank GK, et al. (2019). "Exercise for preventing falls in older people living in the community." \emph{Cochrane Database of Systematic Reviews}, \bold{1}. Art.{\sspace}No.: CD012424. <\doi{10.1002/14651858.CD012424.pub2}>
}
\keyword{dataset}