\name{pb.bayesian.binary}
\alias{pb.bayesian.binary}

\title{
Bayesian Method for Assessing Publication Bias/Small-Study Effects in Meta-Analysis of a Binary Outcome
}
\description{
Performs multiple methods introduced in Shi et al.\sspace(2020) to assess publication bias/small-study effects under the Bayesian framework in a meta-analysis of (log) odds ratios.
}
\usage{
pb.bayesian.binary(n00, n01, n10, n11, p01 = NULL, p11 = NULL, data,
                   sig.level = 0.1,  method = "bay", het = "mul",
                   sd.prior = "unif", n.adapt = 1000, n.chains = 3,
                   n.burnin = 5000, n.iter = 10000, thin = 2,
                   upp.het = 2, phi = 0.5, coda = FALSE,
                   traceplot = FALSE, seed = 1234)
}

\arguments{
  \item{n00}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the counts of non-events in treatment group 0 in the collected studies.
  }
  \item{n01}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the counts of events in treatment group 0 in the collected studies.
  }
  \item{n10}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the counts of non-events in treatment group 1 in the collected studies.
  }
  \item{n11}{
  a numeric vector or the corresponding column name in the argument \code{data}, specifying the counts of events in treatment group 1 in the collected studies. 
  }
  \item{p01}{
  an optional numeric vector specifying true event rates (e.g., from simulations) in the treatment group 0 across studies.
  }
  \item{p11}{
  an optional numeric vector specifying true event rates (e.g., from simulations) in the treatment group 1 across studies.
  }
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{n00}, \code{n01}, \code{n10}, \code{n11}, \code{p01} (if any), and \code{p11} (if any) should be specified as their corresponding column names in \code{data}.
  }
  \item{sig.level}{
  a numeric value specifying the statistical significance level \eqn{\alpha} for testing for publication bias. The default is 0.1. It corresponds to \eqn{(1 - \alpha) \times 100\%} confidence/credible intervals.
  }
  \item{method}{
  a character string specifying the method for assessing publication bias via Bayesian hierarchical models. It can be one of \code{"bay"} (the Bayesian approach proposed in Shi et al., 2020), \code{"reg.bay"} (Egger's regression test, see Egger et al., 1997, under the Bayesian framework) and \code{"smoothed.bay"} (the regression test based on the smoothed sample variance, see Jin et al., 2014, under the Bayesian framework), where all regression tests are under the random-effects setting. The default is \code{"bay"}. 
}
  \item{het}{
  a character string specifying the type of heterogeneity assumption for the publication bias tests. It can be either \code{"mul"} (multiplicative heterogeneity assumption; see Thompson and Sharpe, 1999) or \code{"add"} (additive heterogeneity assumption). The default is \code{"mul"}.
  }
  \item{sd.prior}{
  a character string specifying prior distributions for standard deviation parameters. It can be either \code{"unif"} (uniform distribution) or \code{"hn"} (half-normal distribution). The default is \code{"unif"}.
  }
  \item{n.adapt}{
  the number of iterations for adaptation in the Markov chain Monte Carlo (MCMC) algorithm. The default is 1,000. This argument and the following \code{n.chains}, \code{n.burnin}, \code{n.iter}, and \code{thin} are passed to the functions in the package \pkg{rjags}.
  }
  \item{n.chains}{
  the number of MCMC chains. The default is 1.
  }
  \item{n.burnin}{
  the number of iterations for burn-in period. The default is 5,000.
  }
  \item{n.iter}{
  the total number of iterations in each MCMC chain after the burn-in period. The default is 10,000.
  }
  \item{thin}{
  a positive integer specifying thinning rate. The default is 2.
  }
  \item{upp.het}{
  a positive number for specifying the upper bound of uniform priors for standard deviation parameters (if \code{sd.prior} = \code{"unif"}). The default is 2.
  }
  \item{phi}{
  a positive number for specifying the hyper-parameter of half-normal priors for standard deviation parameters (if \code{sd.prior} = \code{"hn"}). The default is 0.5. 
  }
  \item{coda}{
  a logical value indicating whether to output MCMC posterior samples. The default is \code{FALSE}.
  }
  \item{traceplot}{
  a logical value indicating whether to draw trace plots for the regression slopes. The default is \code{FALSE}.
  }
  \item{seed}{
  an integer for specifying the seed value for reproducibility.
  }
}
\details{
The Bayesian models are specified in Shi et al.\sspace(2020). The vague prior N(0, \eqn{10^4}) is used for the regression intercept and slope, and the uniform prior U(0, \code{upp.het}) and half-normal prior HN(\code{phi}) are used for standard deviation parameters. The half-normal priors may be preferred in meta-analyses with rare events or small sample sizes.
}
\value{
This function returns a list containing estimates of regression slopes and their credible intervals with the specified significance level (\code{sig.level}) as well as MCMC posterior samples (if \code{coda} = \code{TRUE}). Each element name in this list is related to a certain publication bias method (e.g., \code{est.bay} and \code{ci.bay} represent the slope estimate and its credible interval based on the proposed Bayesian method). In addition, traceplots for the regression slope are drawn if \code{traceplot} = \code{TRUE}. 
}
\references{
Egger M, Davey Smith G, Schneider M, Minder C (1997). "Bias in meta-analysis detected by a simple, graphical test." \emph{BMJ}, \bold{315}(7109), 629--634. <\doi{10.1136/bmj.315.7109.629}>

Jin Z-C, Wu C, Zhou X-H, He J (2014). "A modified regression method to test publication bias in meta-analyses with binary outcomes." \emph{BMC Medical Research Methodology}, \bold{14}, 132. <\doi{10.1186/1471-2288-14-132}>

Shi L, Chu H, Lin L (2020). "A Bayesian approach to assessing small-study effects in meta-analysis of a binary outcome with controlled false positive rate". \emph{Research Synthesis Methods}, \bold{11}(4), 535--552. <\doi{10.1002/jrsm.1415}>

Thompson SG, Sharp SJ (1999). "Explaining heterogeneity in meta-analysis: a comparison of methods." \emph{Statistics in Medicine}, \bold{18}(20), 2693--2708. <\doi{10.1002/(SICI)1097-0258(19991030)18:20<2693::AID-SIM235>3.0.CO;2-V}>
}
\author{
Linyu Shi \email{ls16d@my.fsu.edu}
}
\note{
The current version does not support other effect measures such as relative risks or risk differences.
}

\seealso{
\code{\link{pb.hybrid.binary}}, \code{\link{pb.hybrid.generic}}
}
\examples{
\donttest{
data("dat.poole")
set.seed(654321)
## increase n.burnin and n.iter for better convergence of MCMC
rslt.poole <- pb.bayesian.binary(n00, n01, n10, n11, data = dat.poole,
  method = "bay", het = "mul", sd.prior = "unif", n.adapt = 1000,
  n.chains = 3, n.burnin = 500, n.iter = 2000, thin = 2, upp.het = 2)
rslt.poole

data("dat.ducharme")
set.seed(654321)
## increase n.burnin and n.iter for better convergence of MCMC
rslt.ducharme <- pb.bayesian.binary(n00, n01, n10, n11, data = dat.ducharme,
  method = "bay", het = "mul", sd.prior = "unif", n.adapt = 1000,
  n.chains = 3, n.burnin = 500, n.iter = 2000, thin = 2, upp.het = 2)
rslt.ducharme

data("dat.henry")
set.seed(654321)
## increase n.burnin and n.iter for better convergence of MCMC
rslt.henry <- pb.bayesian.binary(n00, n01, n10, n11, data = dat.henry,
  method = "bay", het = "mul", sd.prior = "unif", n.adapt = 1000,
  n.chains = 3, n.burnin = 500, n.iter = 2000, thin = 2, upp.het = 2)
rslt.henry
}
}
\keyword{publication bias/small-study effects}
