\name{nma.predrank}
\alias{nma.predrank}
\title{
Predictive P-Score for Treatment Ranking in Bayesian Network Meta-Analysis
}
\description{
Calculates the P-score and predictive P-score for a network meta-analysis in the Bayesian framework described in Rosenberger et al.{\sspace}(2021).
}
\usage{
nma.predrank(sid, tid, r, n, data, n.adapt = 1000, n.chains = 3, n.burnin = 2000,
             n.iter = 5000, n.thin = 2, lowerbetter = TRUE, pred = TRUE,
             pred.samples = FALSE, trace = FALSE)
}
\arguments{
  \item{sid}{
  a vector specifying the study IDs, from 1 to the number of studies.
}
  \item{tid}{
  a vector specifying the treatment IDs, from 1 to the number of treatments.
}
  \item{r}{
  a numeric vector specifying the event counts.
}
  \item{n}{
  a numeric vector specifying the sample sizes.
}
  \item{data}{
  an optional data frame containing the network meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{sid}, \code{tid}, \code{r}, and \code{n}, should be specified as their corresponding column names in \code{data}.
  }
  \item{n.adapt}{
  the number of iterations for adaptation in the Markov chain Monte Carlo (MCMC) algorithm. The default is 1,000. This argument and the following \code{n.chains}, \code{n.burnin}, \code{n.iter}, and \code{n.thin} are passed to the functions in the package \pkg{rjags}.
}
  \item{n.chains}{
  the number of MCMC chains. The default is 3.
}
  \item{n.burnin}{
  the number of iterations for burn-in period. The default is 2,000.
}
  \item{n.iter}{
  the total number of iterations in each MCMC chain after the burn-in period. The default is 5,000.
}
  \item{n.thin}{
  a positive integer specifying thinning rate. The default is 2.
}
  \item{lowerbetter}{
  A logical value indicating whether lower effect measures indicate better treaetments. If \code{lowerbetter} = \code{TRUE} (the default), then lower effect measures indicate better treatments.
}
  \item{pred}{
  a logical value indicating whether the treatment ranking measures in a new study are to be derived. These measures are only derived when \code{pred} = \code{TRUE}.
}
  \item{pred.samples}{
  a logical value indicating whether the posterior samples of expected scaled ranks in a new study are to be saved.
}
  \item{trace}{
  a logical value indicating whether all posterior samples are to be saved.
}
}
\details{
Under the frequentist setting, the P-score is built on the quantiles
\deqn{P_{kh} = \Phi((\hat{d}_{1k} - \hat{d}_{1h})/s_{kh}),}
where \eqn{\hat{d}_{1k}} and \eqn{\hat{d}_{1h}} are the point estimates of treatment effects for \eqn{k} vs.{\sspace}1 and \eqn{h} vs.{\sspace}1, respectively, and \eqn{s_{kh}} is the standard error of \eqn{\hat{d}_{1k} - \hat{d}_{1h}} (Rucker and Schwarzer, 2015). Moreover, \eqn{\Phi(\cdot)} is the cumulative distribution function of the standard normal distribution. The quantity \eqn{P_{kh}} can be interpreted as the extent of certainty that treatment \eqn{k} is better than \eqn{h}. The frequentist P-score of treatment \eqn{k} is \eqn{\frac{1}{K-1} \sum_{h \neq k} P_{kh}}.

Analogously to the frequentist P-score, conditional on \eqn{d_{1h}} and \eqn{d_{1k}}, the quantities \eqn{P_{hk}} from the Bayesian perspective can be considered as \eqn{I(d_{1k} > d_{1h})}, which are Bernoulli random variables. To quantify the overall performance of treatment \eqn{k}, we may similarly use
\deqn{\bar{P}_k = \frac{1}{K-1} \sum_{h \neq k} I(d_{1k} > d_{1h}).}
Note that \eqn{\bar{P}_k} is a parameter under the Bayesian framework, while the frequentist P-score is a statistic. Moreover, \eqn{\sum_{h \neq k} I(d_{1k} > d_{1h})} is equivalent to \eqn{K - R_k}, where \eqn{R_k} is the true rank of treatment \eqn{k}. Thus, we may also write \eqn{\bar{P}_k = (K - R_k)/(K - 1)}; this corresponds to the findings by Rucker and Schwarzer (2015). Consequently, we call \eqn{\bar{P}_k} the scaled rank in the network meta-analysis (NMA) for treatment \eqn{k}. It transforms the range of the original rank between 1 and \eqn{K} to a range between 0 and 1. In addition, note that \eqn{E[I(d_{1k} > d_{1h} | Data)] = \Pr(d_{1k} > d_{1h} | Data)}, which is analogous to the quantity of \eqn{P_{kh}} under the frequentist framework. Therefore, we use the posterior mean of the scaled rank \eqn{\bar{P}_k} as the Bayesian P-score; it is a counterpart of the frequentist P-score.

The scaled ranks \eqn{\bar{P}_k} can be feasibly estimated via the MCMC algorithm. Let \eqn{\{d_{1k}^{(j)}; k = 2, \ldots, K\}_{j=1}^J} be the posterior samples of the overall relative effects \eqn{d_{1k}} of all treatments vs.{\sspace}the reference treatment 1 in a total of \eqn{J} MCMC iterations after the burn-in period, where \eqn{j} indexes the iterations. As \eqn{d_{11}} is trivially 0, we set \eqn{d_{11}^{(j)}} to 0 for all \eqn{j}. The \eqn{j}th posterior sample of treatment \eqn{k}'s scaled rank is \eqn{\bar{P}_k^{(j)} = \frac{1}{K-1} \sum_{h \neq k}I(d_{1k}^{(j)} > d_{1h}^{(j)})}. We can make inferences for the scaled ranks from the posterior samples \eqn{\{\bar{P}_k^{(j)}\}_{j=1}^{J}}, and use their posterior means as the Bayesian P-scores. We may also obtain the posterior medians as another set of point estimates, and the 2.5\% and 97.5\% posterior quantiles as the lower and upper bounds of 95\% credible intervals (CrIs), respectively. Because the posterior samples of the scaled ranks take discrete values, the posterior medians and the CrI bounds are also discrete.

Based on the idea of the Bayesian P-score, we can similarly define the predictive P-score for a future study by accounting for the heterogeneity between the existing studies in the NMA and the new study. Specifically, we consider the probabilities in the new study
\deqn{P_{new,kh} = \Pr(\delta_{new,1k} > \delta_{new,1h}),}
conditional on the population parameters \eqn{d_{1h}}, \eqn{d_{1k}}, and \eqn{\tau} from the NMA. Here, \eqn{\delta_{new,1k}} and \eqn{\delta_{new,1h}} represent the treatment effects of \eqn{k} vs.{\sspace}1 and \eqn{h} vs.{\sspace}1 in the new study, respectively. The \eqn{P_{new,kh}} corresponds to the quantity \eqn{P_{kh}} in the NMA; it represents the probability of treatment \eqn{k} being better than \eqn{h} in the new study. Due to heterogeneity, \eqn{\delta_{new,1k} \sim N(d_{1k}, \tau^2)} and \eqn{\delta_{new,1h} \sim N(d_{1h}, \tau^{2})}. The correlation coefficients between treatment comparisons are typically assumed to be 0.5; therefore, such probabilities in the new study can be explicitly calculated as \eqn{P_{new,kh} = \Phi((d_{1k} - d_{1h})/\tau)}, which is a function of \eqn{d_{1h}}, \eqn{d_{1k}}, and \eqn{\tau}. Finally, we use
\deqn{\bar{P}_{new,k} = \frac{1}{K-1} \sum_{h \neq k} P_{new,kh}}
to quantify the performance of treatment \eqn{k} in the new study. The posterior samples of \eqn{\bar{P}_{new,k}} can be derived from the posterior samples of \eqn{d_{1k}}, \eqn{d_{1h}}, and \eqn{\tau} during the MCMC algorithm.

Note that the probabilities \eqn{P_{new,kh}} can be written as \eqn{E[I(\delta_{new,1k} > \delta_{new,1h})]}. Based on similar observations for the scaled ranks in the NMA, the \eqn{\bar{P}_{new,k}} in the new study subsequently becomes
\deqn{\bar{P}_{new,k} = \frac{1}{K-1} E\left[\sum_{h \neq k} I(\delta_{new,1k} > \delta_{new,1h})\right] = E\left[\frac{K - R_{new,k}}{K - 1}\right],}
where \eqn{R_{new,k}} is the true rank of treatment \eqn{k} in the new study. Thus, we call \eqn{\bar{P}_{new,k}} the expected scaled rank in the new study. Like the Bayesian P-score, we define the predictive P-score as the posterior mean of \eqn{\bar{P}_{new,k}}. The posterior medians and 95\% CrIs can also be obtained using the MCMC samples of \eqn{\bar{P}_{new,k}}. See more details in Rosenberger et al.{\sspace}(2021).
}

\value{
This function estimates the P-score for all treatments in a Bayesian NMA, estimates the predictive P-score (if \code{pred} = \code{TRUE}), gives the posterior samples of expected scaled ranks in a new study (if \code{pred.samples} = \code{TRUE}), and outputs all MCMC posterior samples (if \code{trace} = \code{TRUE}).
} 

\references{
Rosenberger KJ, Duan R, Chen Y, Lin L (2021). "Predictive P-score for treatment ranking in Bayesian network meta-analysis." \emph{BMC Medical Research Methodology}, \bold{21}, 213. <\doi{10.1186/s12874-021-01397-5}>

Rucker G, Schwarzer G (2015). "Ranking treatments in frequentist network meta-analysis works without resampling methods." \emph{BMC Medical Research Methodology}, \bold{15}, 58. <\doi{10.1186/s12874-015-0060-8}>
}

\author{
Kristine J.{\sspace}Rosenberger, Lifeng Lin 
}

\examples{
\donttest{
## increase n.burnin (e.g., to 50000) and n.iter (e.g., to 200000)
## for better convergence of MCMC
data("dat.sc")
set.seed(1234)
out1 <- nma.predrank(sid, tid, r, n, data = dat.sc, n.burnin = 500, n.iter = 2000,
  lowerbetter = FALSE, pred.samples = TRUE)
out1$P.score
out1$P.score.pred

cols <- c("red4", "plum4", "paleturquoise4", "palegreen4")
cols.hist <- adjustcolor(cols, alpha.f = 0.4)
trtnames <- c("1) No contact", "2) Self-help", "3) Individual counseling",
  "4) Group counseling")
brks <- seq(0, 1, 0.01)
hist(out1$P.pred[[1]], breaks = brks, freq = FALSE,
  xlim = c(0, 1), ylim = c(0, 5), col = cols.hist[1], border = cols[1],
  xlab = "Expected scaled rank in a new study", ylab = "Density", main = "")
hist(out1$P.pred[[2]], breaks = brks, freq = FALSE,
  col = cols.hist[2], border = cols[2], add = TRUE)
hist(out1$P.pred[[3]], breaks = brks, freq = FALSE,
  col = cols.hist[3], border = cols[3], add = TRUE)
hist(out1$P.pred[[4]], breaks = brks, freq = FALSE,
  col = cols.hist[4], border = cols[4], add = TRUE)
legend("topright", fill = cols.hist, border = cols, legend = trtnames)


data("dat.xu")
set.seed(1234)
out2 <- nma.predrank(sid, tid, r, n, data = dat.xu, n.burnin = 500, n.iter = 2000)
out2
}
}
\keyword{multivariate meta-analysis}