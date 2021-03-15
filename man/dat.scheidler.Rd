\name{dat.scheidler}
\alias{dat.scheidler}
\docType{data}
\title{
Meta-Analysis on the Utility of Lymphangiography, Computed Tomography, and Magnetic Resonance Imaging for the Diagnosis of Lymph Node Metastasis
}
\description{
This meta-analysis serves as an example of meta-analyses of diagnostic tests.
}
\usage{data("dat.scheidler")}
\format{
  A data frame with 44 studies with the following 5 variables; each row represents a study.
  \describe{
    \item{\code{dt}}{types of diagnostic tests; CT: computed tomography; LAG: lymphangiography; and MRI: magnetic resonance imaging.}
    \item{\code{tp}}{counts of true positives.}
    \item{\code{fp}}{counts of false positives.}
    \item{\code{fn}}{counts of false negatives.}
    \item{\code{tn}}{counts of true negatives.}
  }
}
\source{
Scheidler J, Hricak H, Yu KK, Subak L, Segal MR (1997). "Radiological evaluation of lymph node metastases in patients with cervical cancer: a meta-analysis." \emph{JAMA}, \bold{278}(13), 1096--1101. <\doi{10.1001/jama.1997.03550130070040}>
}
\keyword{dataset}