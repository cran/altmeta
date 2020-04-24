\name{dat.williams}
\alias{dat.williams}
\docType{data}
\title{
A Meta-Analysis on the Effect of Pharmacotherapy for Social Anxiety Disorder
}
\description{
This dataset serves as an example of meta-analysis of (log) relative risks.
}
\usage{data("dat.williams")}
\format{
  A data frame with 20 studies with the following 7 variables within each study.
  \describe{
    \item{\code{y}}{point esimates of log relative risks.}
    \item{\code{s2}}{sample variances of log relative risks.}
    \item{\code{n1}}{sample sizes in treatment group 1 (medication).}
    \item{\code{n2}}{sample sizes in treatment group 2 (placebo).}
    \item{\code{r1}}{event counts in treatment group 1.}
    \item{\code{r2}}{event counts in treatment group 2.}
    \item{\code{n}}{total sample sizes.}
  }
}
\source{
Williams T, Hattingh CJ, Kariuki CM, Tromp SA, van Balkom AJ, Ipser JC, Stein DJ (2017). "Pharmacotherapy for social anxiety disorder (SAnD)." \emph{Cochrane Database of Systematic Reviews}, \bold{10}, Art.{\sspace}No.: CD001206. <\doi{10.1002/14651858.CD001206.pub3}>
}
\keyword{datasets}
