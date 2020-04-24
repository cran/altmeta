\name{dat.pte}
\alias{dat.pte}
\docType{data}
\title{
Meta-Analysis of Multiple Risk Factors for Pterygium
}
\description{
This dataset serves as an example to illustrate network meta-analysis of multiple factors. It consists of 29 studies on a total of 8 risk factors: area of residence (rural vs.{\sspace}urban); education attainment (low vs.{\sspace}high); latitude of residence (low vs.{\sspace}high); occupation type (outdoor vs.{\sspace}indoor); smoking status (yes vs.{\sspace}no); use of hat (yes vs.{\sspace}no); use of spectacles (yes vs.{\sspace}no); and use of sunglasses (yes vs.{\sspace}no). Each study only investigates a subset of the 8 risk factors, so the dataset contains many missing values.
}
\usage{data("dat.pte")}
\format{
  A list containing two elements, \code{y} and \code{se}.
  \describe{
    \item{\code{y}}{a 29 x 8 numeric matrix containing the observed effect sizes; the rows represent studies and the columns represent outcomes.}
    \item{\code{se}}{a 29 x 8 numeric matrix containing the within-study standard errors; the rows represent studies and the columns represent outcomes.}
  }
}
\source{
Serghiou S, Patel CJ, Tan YY, Koay P, Ioannidis JPA (2016). "Field-wide meta-analyses of observational associations can map selective availability of risk factors and the impact of model specifications." \emph{Journal of Clinical Epidemiology}, \bold{71}, 58--67. <\doi{10.1016/j.jclinepi.2015.09.004}>
}
\keyword{datasets}
