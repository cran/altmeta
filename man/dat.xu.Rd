\name{dat.xu}
\alias{dat.xu}
\docType{data}
\title{
A Network Meta-Analysis on Immune Checkpoint Inhibitor Drugs
}
\description{
This network meta-analysis investigates the effects of seven immune checkpoint inhibitor (ICI) drugs on all-grade treatment-related adverse events (TrAEs). It aimed to provide a safety ranking of the ICI drugs for the treatment of cancer.
}
\usage{data("dat.xu")}
\format{
  A dataset of network meta-analysis with binary outcomes, containing 23 studies and 7 treatments.
  \describe{
    \item{\code{sid}}{study IDs.}
    \item{\code{tid}}{treatment IDs.}
    \item{\code{r}}{event counts.}
    \item{\code{n}}{sample sizes.}
  }
}
\details{
Treatment IDs represent: 1) conventional therapy; 2) nivolumab; 3) pembrolizumab; 4) two ICIs; 5) ICI and conventional therapy; 6) atezolizumab; and 7) ipilimumab.
}
\source{
Xu C, Chen YP, Du XJ, Liu JQ, Huang CL, Chen L, Zhou GQ, Li WF, Mao YP, Hsu C, Liu Q, Lin AH, Tang LL, Sun Y, Ma J (2018). "Comparative safety of immune checkpoint inhibitors in cancer: systematic review and network meta-analysis." \emph{BMJ}, \bold{363}, k4226. <\doi{10.1136/bmj.k4226}>
}
\keyword{dataset}