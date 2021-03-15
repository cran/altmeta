\name{dat.annane}
\alias{dat.annane}
\docType{data}
\title{
A Meta-Analysis for Comparing the Effect of Steroids vs. Control in the Length of Intensive Care Unit (ICU) Stay
}
\description{
This dataset serves as an example of meta-analysis of mean differences.
}
\usage{data("dat.annane")}
\format{
  A data frame with 12 studies with the following 5 variables within each study.
  \describe{
    \item{\code{y}}{point esimates of mean differences.}
    \item{\code{s2}}{sample variances of mean differences.}
    \item{\code{n1}}{sample sizes in treatment group 1 (steroids).}
    \item{\code{n2}}{sample sizes in treatment group 2 (control).}
    \item{\code{n}}{total sample sizes.}
  }
}
\source{
Annane D, Bellissant E, Bollaert PE, Briegel J, Keh D, Kupfer Y (2015). "Corticosteroids for treating sepsis." \emph{Cochrane Database of Systematic Reviews}, \bold{12}, Art.{\sspace}No.: CD002243. <\doi{10.1002/14651858.CD002243.pub3}>
}
\keyword{dataset}
