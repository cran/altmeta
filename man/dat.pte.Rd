\name{dat.pte}
\alias{dat.pte}
\docType{data}
\title{
Meta-Analysis of Multiple Risk Factors for Pterygium
}
\description{
This dataset serves as an example to illustrate network meta-analysis of multiple factors. It consists of 29 studies on a total of 8 risk factors: area of residence (rural versus urban); education attainment (low versus high); latitude of residence (low versus high); occupation type (outdoor versus indoor); smoking status (yes versus no); use of hat (yes versus no); use of spectacles (yes versus no); and use of sunglasses (yes versus no). Each study only investigates a subset of the 8 risk factors, so the dataset contains many missing values.
}
\usage{data("dat.pte")}
\format{
  A list containing two elements, \code{y} and \code{se}.
  \describe{
    \item{\code{y}}{A 29 x 8 numeric matrix containing the observed effect sizes; the rows represent studies and the columns represent outcomes.}
    \item{\code{se}}{A 29 x 8 numeric matrix containing the within-study standard errors; the rows represent studies and the columns represent outcomes.}
  }
}
\source{
Serghiou S, Patel CJ, Tan YY, Koay P, and Ioannidis JPA (2016). "Field-wide meta-analyses of observational associations can map selective availability of risk factors and the impact of model specifications." \emph{Journal of Clinical Epidemiology}, \bold{71}, 58--67. <doi:10.1016/j.jclinepi.2015.09.004>
}
\keyword{datasets}
