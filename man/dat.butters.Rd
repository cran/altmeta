\name{dat.butters}
\alias{dat.butters}
\docType{data}
\title{
A Meta-Analysis on the Overall Response of the Addition of Drugs to a Chemotherapy Regimen for Metastatic Breast Cancer
}
\description{
This dataset serves as an example of meta-analysis of (log) odds ratios.
}
\usage{data("dat.butters")}
\format{
  A data frame with 16 studies with the following 7 variables within each study.
  \describe{
    \item{\code{y}}{point esimates of log odds ratios.}
    \item{\code{s2}}{sample variances of log odds ratios.}
    \item{\code{n1}}{sample sizes in treatment group 1 (addition of drug).}
    \item{\code{n2}}{sample sizes in treatment group 2 (control).}
    \item{\code{r1}}{event counts in treatment group 1.}
    \item{\code{r2}}{event counts in treatment group 2.}
    \item{\code{n}}{total sample sizes.}
  }
}
\source{
Butters DJ, Ghersi D, Wilcken N, Kirk SJ, Mallon PT (2010). "Addition of drug/s to a chemotherapy regimen for metastatic breast cancer." \emph{Cochrane Database of Systematic Reviews}, \bold{11}, Art.{\sspace}No.: CD003368. <\doi{10.1002/14651858.CD003368.pub3}>
}
\keyword{dataset}
