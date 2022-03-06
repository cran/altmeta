\name{dat.sc}
\alias{dat.sc}
\docType{data}
\title{
A Network Meta-Analysis on Smoking Cessation
}
\description{
The dataset is extracted from Lu and Ades (2006); it was initially reported by Hasselblad (1998) (without performing a formal network meta-analysis).
}
\usage{data("dat.sc")}
\format{
  A dataset of network meta-analysis with binary outcomes, containing 24 studies and 4 treatments.
  \describe{
    \item{\code{sid}}{study IDs.}
    \item{\code{tid}}{treatment IDs.}
    \item{\code{r}}{event counts.}
    \item{\code{n}}{sample sizes.}
  }
}
\details{
Treatment IDs represent: 1)no contact; 2) selfhelp; 3) individual counseling; and 4) group counseling.
}
\source{
Hasselblad V (1998). "Meta-analysis of multitreatment studies." \emph{Medical Decision Making}, \bold{18}(1), 37--43. <\doi{10.1177/0272989X9801800110}>

Lu G, Ades AE (2006). "Assessing evidence inconsistency in mixed treatment comparisons." \emph{Journal of the American Statistical Association}, \bold{101}(474), 447--459. <\doi{10.1198/016214505000001302}>
}
\keyword{dataset}