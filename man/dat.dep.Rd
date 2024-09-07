\name{dat.dep}
\alias{dat.dep}
\docType{data}
\title{
A Meta-Analysis of Binary and Continuous Outcomes on Depression
}
\description{
This dataset serves as an example of meta-analysis of combining standardized mean differences and odds ratios.
}
\usage{data("dat.dep")}
\format{
  A data frame with 6 studies with the following 15 variables within each study.
  \describe{
    \item{\code{author}}{The first author of each study.}
    \item{\code{year}}{The publication year of each study.}
    \item{\code{treatment}}{The treatment group.}
    \item{\code{control}}{The control group.}
    \item{\code{y1}}{The sample mean in the treatment group for the continuous outcome.}
    \item{\code{sd1}}{The sample standard deviation in the treatment group for the continuous outcome.}
    \item{\code{n1}}{The sample size in the treatment group for the continuous outcome.}
    \item{\code{y0}}{The sample mean in the control group for the continuous outcome.}
    \item{\code{sd0}}{The sample standard deviation in the control group for the continuous outcome.}
    \item{\code{n0}}{The sample size in the control group for the continuous outcome.}
    \item{\code{r1}}{The event count in the treatment group for the binary outcome.}
    \item{\code{m1}}{The sample size in the treatment group for the binary outcome.}
    \item{\code{r0}}{The event count in the control group for the binary outcome.}
    \item{\code{m0}}{The sample size in the control group for the binary outcome.}
    \item{\code{id.bin}}{An indicator of whether the outcome is binary (1) or continuous (0).}
  }
}
\details{
This dataset is from Cipriani et al.\sspace(2016), comparing the efficacy and tolerability of antidepressants for major depressive disorders in children and adolescents. Our case study focuses on efficacy. The authors originally performed a network meta-analysis; however, here we restrict the comparison to fluoxetine and placebo. The continuous outcomes are measured by the mean overall changes in depressive symptoms from baseline to endpoint. For the binary outcomes, events are defined as whether patients' depression rating scores were reduced by at least a specified cutoff value.
}
\source{
Cipriani A, Zhou X, Del Giovane C, Hetrick SE, Qin B, Whittington C, Coghill D, Zhang Y, Hazell P, Leucht S, Cuijpers P, Pu J, Cohen D, Ravindran AV, Liu Y, Michael KD, Yang L, Liu L, Xie P (2016). "Comparative efficacy and tolerability of antidepressants for major depressive disorder in children and adolescents: a network meta-analysis." \emph{The Lancet}, \bold{388}(10047), 881--890. <\doi{10.1016/S0140-6736(16)30385-3}>
}
\keyword{datasets}
