\name{dat.whiting}
\alias{dat.whiting}
\docType{data}
\title{
A Meta-Analysis on Adverse Events for the Comparison Cannabinoid vs. Placebo
}
\description{
This dataset serves as an example of meta-analysis of (log) odds ratios.
}
\usage{data("dat.whiting")}
\format{
  A data frame with 29 studies with the following 9 variables within each study.
  \describe{
    \item{\code{y}}{point esimates of log odds ratios.}
    \item{\code{s2}}{sample variances of log odds ratios.}
    \item{\code{n00}}{counts of non-events in treatment group 0 (placebo).}
    \item{\code{n01}}{counts of events in treatment group 0.}
    \item{\code{n10}}{counts of non-events in treatment group 1 (cannabinoid).}
    \item{\code{n11}}{counts of events in treatment group 1 (cannabinoid).}
    \item{\code{n0}}{sample sizes in treatment group 0.}
    \item{\code{n1}}{sample sizes in treatment group 1.}
    \item{\code{n}}{total sample sizes.}
  }
}
\source{
Whiting PF, Wolff RF, Deshpande S, Di Nisio M, Duffy S, Hernandez AV, Keurentjes JC, Lang S, Misso K, Ryder S, Schmidlkofer S, Westwood M, Kleijnen J (2015). "Cannabinoids for medical use: a systematic review and meta-analysis." \emph{JAMA}, \bold{313}(24), 2456--2473. <\doi{10.1001/jama.2015.6358}>
}
\keyword{dataset}