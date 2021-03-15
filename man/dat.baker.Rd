\name{dat.baker}
\alias{dat.baker}
\docType{data}
\title{
A Network Meta-Analysis on Effects of Pharmacologic Treatments for Chronic Obstructive Pulmonary Disease
}
\description{
This dataset serves as an example of network meta-analysis with binary outcomes.
}
\usage{data("dat.baker")}
\format{
  A dataset of network meta-analysis with binary outcomes, containing 38 studies and 5 treatments.
  \describe{
    \item{\code{sid}}{study IDs.}
    \item{\code{tid}}{treatment IDs.}
    \item{\code{r}}{event counts.}
    \item{\code{n}}{sample sizes.}
  }
}
\details{
Treatment IDs represent: 1) placebo; 2) inhaled corticosteroid; 3) inhaled corticosteroid + long-acting \eqn{\beta_2}-agonist; 4) long-acting \eqn{\beta_2}-agonist; and 5) tiotropium.
}
\source{
Baker WL, Baker EL, Coleman CI (2009). "Pharmacologic treatments for chronic obstructive pulmonary disease: a mixed-treatment comparison meta-analysis." \emph{Pharmacotherapy}, \bold{29}(8), 891--905. <\doi{10.1592/phco.29.8.891}>
}
\keyword{dataset}