\name{dat.fib}
\alias{dat.fib}
\docType{data}
\title{
A Multivariate Meta-Analysis by the Fibrinogen Studies Collaboration
}
\description{
This multivariate meta-analysis serves as an example to illustrate function usage in the package \pkg{altmeta}. It consists of 31 studies with 4 outcomes.
}
\usage{data("dat.fib")}
\format{
  A list containing three elements, \code{y}, \code{S}, and \code{sd}.
  \describe{
    \item{\code{y}}{a 31 x 4 numeric matrix containing the observed effect sizes; the rows represent studies and the columns represent outcomes.}
    \item{\code{S}}{a list containing 31 elements; each element is within-study covariance matrix of the corresponding study.}
    \item{\code{sd}}{a 31 x 4 numeric matrix containing the within-study standard deviations; the rows represent studies and the columns represent outcomes.}
  }
}
\source{
Fibrinogen Studies Collaboration (2004). "Collaborative meta-analysis of prospective studies of plasma fibrinogen and cardiovascular disease." \emph{European Journal of Cardiovascular Prevention and Rehabilitation}, \bold{11}(1), 9--17. <\doi{10.1097/01.hjr.0000114968.39211.01}>

Fibrinogen Studies Collaboration (2005). "Plasma fibrinogen level and the risk of major cardiovascular diseases and nonvascular mortality: an individual participant meta-analysis." \emph{JAMA}, \bold{294}(14), 1799--1809. <\doi{10.1001/jama.294.14.1799}>
}
\keyword{dataset}