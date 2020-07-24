\name{dat.poole}
\alias{dat.poole}
\docType{data}
\title{
A Meta-Analysis for Evaluating the Effect of Mucolytic on Bronchitis/Chronic Obstructive Pulmonary Disease
}
\description{
This meta-analysis serves as an example of meta-analysis with binary outcomes.
}
\usage{data("dat.poole")}
\format{
  A data frame containing 24 studies with the following 4 variables within each study.
  \describe{
    \item{\code{n00}}{counts of non-events in treatment group 0 (placebo).}
    \item{\code{n01}}{counts of events in treatment group 0 (placebo).}
    \item{\code{n10}}{counts of non-events in treatment group 1 (mucolytic).}
    \item{\code{n11}}{counts of events in treatment group 1 (mucolytic).}
  }
}
\source{
Poole P, Chong J, Cates CJ (2015). "Mucolytic agents versus placebo for chronic bronchitis or chronic obstructive pulmonary disease." \emph{Cochrane Database of Systematic Reviews}, \bold{7}, Art.{\sspace}No.: CD001287. <\doi{10.1002/14651858.CD001287.pub5}>
}
\keyword{datasets}