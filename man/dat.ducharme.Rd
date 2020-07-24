\name{dat.ducharme}
\alias{dat.ducharme}
\docType{data}
\title{
A Meta-Analysis on the Effect of Long-Acting Inhaled Beta2-Agonists vs. Control for Chronic Asthma
}
\description{
This meta-analysis serves as an example of meta-analysis with binary outcomes.
}
\usage{data("dat.ducharme")}
\format{
  A data frame containing 33 studies with the following 4 variables within each study.
  \describe{
    \item{\code{n00}}{counts of non-events in treatment group 0 (placebo).}
    \item{\code{n01}}{counts of events in treatment group 0 (placebo).}
    \item{\code{n10}}{counts of non-events in treatment group 1 (beta2-agonists).}
    \item{\code{n11}}{counts of events in treatment group 1 (beta2-agonists).}
  }
}
\note{The original review collected 35 studies; two double-zero-counts studies are excluded from this dataset because their odds ratios are not estimable.}
\source{
Ducharme FM, Ni Chroinin M, Greenstone I, Lasserson TJ (2010). "Addition of long-acting beta2-agonists to inhaled corticosteroids versus same dose inhaled corticosteroids for chronic asthma in adults and children." \emph{Cochrane Database of Systematic Reviews}, \bold{5}, Art.{\sspace}No.: CD005535. <\doi{10.1002/14651858.CD005535.pub2}>
}
\keyword{datasets}