\name{dat.henry}
\alias{dat.henry}
\docType{data}
\title{
A Meta-Analysis for Evaluating the Effect of Tranexamic Acid on Perioperative Allogeneic Blood Transfusion
}
\description{
This meta-analysis serves as an example of meta-analysis with binary outcomes.
}
\usage{data("dat.henry")}
\format{
  A data frame containing 26 studies with the following 4 variables within each study.
  \describe{
    \item{\code{n00}}{counts of non-events in treatment group 0 (placebo).}
    \item{\code{n01}}{counts of events in treatment group 0 (placebo).}
    \item{\code{n10}}{counts of non-events in treatment group 1 (tranexamic acid).}
    \item{\code{n11}}{counts of events in treatment group 1 (tranexamic acid).}
  }
}
\note{The original review collected 27 studies; one double-zero-counts study is excluded from this dataset because its odds ratio is not estimable.}
\source{
Henry DA, Carless PA, Moxey AJ, O'Connell, Stokes BJ, Fergusson DA, Ker K (2011). "Anti-fibrinolytic use for minimising perioperative allogeneic blood transfusion." \emph{Cochrane Database of Systematic Reviews}, \bold{1}, Art.{\sspace}No.: CD001886. <\doi{10.1002/14651858.CD001886.pub3}>
}
\keyword{dataset}