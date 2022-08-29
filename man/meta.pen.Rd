\name{meta.pen}
\alias{meta.pen}
\title{
A penalization approach to random-effects meta-analysis
}
\description{
Performs the penalization methods introduced in Wang et al.{\sspace}(2022) to achieve a compromise between the common-effect and random-effects model.
}
\usage{
meta.pen(y, s2, data, tuning.para = "tau", upp = 1, n.cand = 100, tol = 1e-10)
}
\arguments{
  \item{y}{
  a numeric vector or the corresponding column name in the argument data, specifying the observed effect sizes in the collected studies.
}
  \item{s2}{
  a numeric vector or the corresponding column name in the argument data, specifying the within-study variances.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{y} and \code{s2}, should be specified as their corresponding column names in \code{data}.
}
  \item{tuning.para}{
  a character string specifying the type of tuning parameter used in the penalization method. It should be one of "lambda" (use lambda as a tuning parameter) or "tau" (use the standard deviation as a tuning parameter). The default is "tau".
}
  \item{upp}{
  a positive scalar used to control the upper bound of the range for the tuning parameter. Specifically, [0, T*\code{upp}] is used as the range of a tuning parameter. T is the upper threshold value of a tuning parameter. The default value of \code{upp} is 1. 
}
  \item{n.cand}{
  the total number of candidate values of the tuning parameter within the specified range. The default is 100. 
}
  \item{tol}{
  the desired accuracy (convergence tolerance). The default is 1e-10.
}
}
\details{
Suppose a meta-analysis collects \eqn{n} independent studies. Let \eqn{\mu_{i}} be the true effect size in study \eqn{i} (\eqn{i} = 1, \ldots, \eqn{n}). Each study reports an estimate of the effect size and its sample variance, denoted by \eqn{y_{i}} and \eqn{s_{i}^{2}}, respectively. These data are commonly modeled as \eqn{y_{i}} from \eqn{N(\mu_{i},s_{i}^{2})}. If study-specific true effect sizes \eqn{\mu_{i}} are assumed i.i.d.{\sspace}from \eqn{N(\mu, \tau^{2})}, this is the random-effects (RE) model, where \eqn{\mu} is the overall effect size and \eqn{\tau^{2}} is the between-study variance. If \eqn{\tau^{2}=0} and thus \eqn{\mu_{i}=\mu} for all studies, this implies that studies are homogeneous and the RE model is reduced to the common-effect (CE) model.

Marginally, the RE model yields \eqn{y_{i} \sim N(\mu,s_{i}^2+\tau^{2})}, and its log-likelihood is 
\deqn{l(\mu, \tau^{2}) = -\frac{1}{2} \sum_{i=1}^{n} \left[\log(s_{i}^{2} + \tau^{2}) + \frac{(y_{i} - \mu)^2}{s_{i}^2 + \tau^2}\right] + C,}
where \eqn{C} is a constant. In the past two decades, penalization methods have been rapidly developed for variable selection in high-dimensional data analysis to control model complexity and reduce the variance of parameter estimates. Borrowing the idea from the penalization methods, we employ a penalty term on the between-study variance \eqn{\tau^2} when the heterogeneity is overestimated. The penalty term increases with \eqn{\tau^2}. Specifically, we consider the following optimization problem:
\deqn{(\hat{\mu}(\lambda), \hat{\tau}^2(\lambda)) = \min_{\mu, \tau^2 \geq 0} \left\{\sum_{i=1}^{n} \left[ \log(s_{i}^{2} + \tau^{2}) + \frac{(y_{i} - \mu)^2}{s_{i}^2 + \tau^2}\right] + \lambda \tau^2\right\},}
where \eqn{\lambda \geq 0} is a tuning parameter that controls the penalty strength. Using the technique of profile likelihood by taking the target function's derivative in the above equation with respect to \eqn{\mu} for a given \eqn{\tau^2}. The bivariate optimization problem is reduced to a univariate minimization problem. When \eqn{\lambda=0}, the minimization problem is equivalent to minimizing the log-likelihood without penalty, so the penalized-likelihood method is identical to the conventional RE model. By contrast, it can be shown that a sufficiently large \eqn{\lambda} produces the estimated between-study variance as 0, leading to the conventional CE model.

As different tuning parameters lead to different estimates of \eqn{\hat{\mu}(\lambda)} and \eqn{\hat{\tau}^2(\lambda)}, it is important to select the optimal \eqn{\lambda} among a set of candidate values. We perform the cross-validation process and construct a loss function of \eqn{\lambda} to measure the performance of specific \eqn{\lambda} values. The \eqn{\lambda} corresponding to the smallest loss is considered optimal. The threshold, denoted by \eqn{\lambda_{\max}}, based on the penalty function \eqn{p(\tau^2)=\tau^2} can be calculated. For all \eqn{\lambda > \lambda_{\max}}, the estimated between-study variance is 0. Consequently, we select a certain number of candidate values (e.g., 100) from the range \eqn{[0,\lambda_{\max}]} for the tuning parameter. For a set of tuning parameters, the leave-one-study-out (i.e., \eqn{n}-fold) cross-validation is used to construct the loss function. Specifically, we use the following loss function for the penalization method by tuning \eqn{\lambda}:
\deqn{\hat{L}(\lambda) = \left[\frac{1}{n} \sum_{i=1}^{n} \frac{(y_{i} - \hat{\mu}_{(-i)}(\lambda))^2}{s_{i}^2 + \hat{\tau}_{RE(-i)}^2 + \frac{\sum_{j \ne i}(s_{j}^2 + \hat{\tau}_{RE(-i)}^2) / (s_{j}^2 + \hat{\tau}^2_{(-i)}(\lambda))^2}{(\sum_{j \ne i} 1 / (s_{j}^2 + \hat{\tau}^2_{(-i)}(\lambda)))^2}}\right]^{1/2},}
where the subscript \eqn{(-i)} indicates that study \eqn{i} is removed, \eqn{\hat{\tau}^2_{(-i)}(\lambda)} is the estimated between-study variance for a given \eqn{\lambda}, \eqn{\hat{\tau}_{RE(-i)}^2} is the corresponding between-study variance estimate of the RE model, and 
\deqn{\hat{\mu}_{(-i)}(\lambda) = \frac{\sum_{j \ne i} y_{j} / (s_{j}^2 + \hat{\tau}^2_{(-i)}(\lambda))}{\sum_{j \ne i} 1 / (s_{j}^2 + \hat{\tau}^2_{(-i)}(\lambda))}.}

The above procedure focuses on tuning the parameter, \eqn{\lambda}, to control the penalty strength for the between-study variance. Alternatively, for the purpose of shrinking the potentially overestimated heterogeneity, we can directly treat the between-study standard deviation (SD), \eqn{\tau}, as the tuning parameter. A set of candidate values of \eqn{\tau} are considered, and the value that produces the minimum loss function is selected. Compared with tuning \eqn{\lambda} from the perspective of penalized likelihood, tuning \eqn{\tau} is more straightforward and intuitive from the practical perspective. The candidate values of \eqn{\tau} can be naturally chosen from \eqn{[0,\hat{\tau}_{RE}]}, with the lower and upper bounds corresponding to the CE and RE models, respectively. Denoted the candidate SDs as \eqn{\tau_{t}}, the loss function with respect to \eqn{\tau_{t}} is similarly defined as
\deqn{\hat{L}(\tau_{t}) = \left[\frac{1}{n} \sum_{i=1}^{n} \frac{(y_i - \hat{\mu}_{(-i)}(\tau_{t}))^2}{s_{i}^2 + \hat{\tau}_{RE(-i)} + \frac{\sum_{j \ne i}(s_{j}^2 + \hat{\tau}^2_{RE(-i)}) / (s_{j}^2 + \tau_{t}^2)^2}{[\sum_{j \ne i} 1 / (s_{j}^2 + \tau_{t}^2)]^2}}\right]^{1/2},}
where the overall effect size estimate (excluding study \eqn{i}) is
\deqn{\hat{\mu}_{(-i)}(\tau_{t}) = \frac{\sum_{j \ne i} y_j / (s_{j}^2 + \tau_{t}^2)}{\sum_{j \ne i} 1 / (s_{j}^2 + \tau_{t}^2)}.}
}
\value{
This function returns a list containing estimates of the overall effect size and their 95\% confidence intervals. Specifically, the components include:
  \item{n}{the number of studies in the meta-analysis.}
  \item{I2}{the \eqn{I^2} statistic for quantifying heterogeneity.}
  \item{tau2.re}{the maximum likelihood estimate of the between-study variance.}
  \item{mu.fe}{the estimated overall effect size of the \code{CE} model.}
  \item{se.fe}{the standard deviation of the overall effect size estimate of the \code{CE} model.}
  \item{mu.re}{the estimated overall effect size of the \code{RE} model.}
  \item{se.re}{the standard deviation of the overall effect size estimate of the \code{RE} model.}
  \item{loss}{the values of the loss function for candidate tuning parameters.}
  \item{tau.cand}{the candidate values of the tuning parameter \eqn{\tau}.}
  \item{lambda.cand}{the candidate values of the tuning parameter \eqn{\lambda}.}
  \item{tau.opt}{the estimated between-study standard deviation of the penalization method.}
  \item{mu.opt}{the estimated overall effect size of the penalization method.}
  \item{se.opt}{the standard error estimate of the estimated overall effect size of the penalization method.}
}
\references{
Wang Y, Lin L, Thompson CG, Chu H (2022). "A penalization approach to random-effects meta-analysis." \emph{Statistics in Medicine}, \bold{41}(3), 500--516. <\doi{10.1002/sim.9261}>
}
\author{
Yipeng Wang \email{yipeng.wang1@ufl.edu}
}
\examples{
data("dat.bohren") ## log odds ratio
## perform the penaliztion method by tuning tau
out11 <- meta.pen(y, s2, dat.bohren)
## plot the loss function and candidate taus
plot(out11$tau.cand, out11$loss, xlab = NA, ylab = NA, 
  lwd = 1.5, type = "l", cex.lab = 0.8, cex.axis = 0.8)
title(xlab = expression(paste(tau[t])),
  ylab = expression(paste("Loss function ", ~hat(L)(tau[t]))),
  cex.lab = 0.8, line = 2)
idx <- which(out11$loss == min(out11$loss))
abline(v = out11$tau.cand[idx], col = "gray", lwd = 1.5, lty = 2)
     
## perform the penaliztion method by tuning lambda     
out12 <- meta.pen(y, s2, dat.bohren, tuning.para = "lambda")
## plot the loss function and candidate lambdas
plot(log(out12$lambda.cand + 1), out12$loss, xlab = NA, ylab = NA, 
  lwd=1.5, type = "l", cex.lab = 0.8, cex.axis = 0.8)
title(xlab = expression(log(lambda + 1)),
  ylab = expression(paste("Loss function ", ~hat(L)(lambda))),
  cex.lab = 0.8, line = 2)
idx <- which(out12$loss == min(out12$loss))
abline(v = log(out12$lambda.cand[idx] + 1), col = "gray", lwd = 1.5, lty = 2)

\donttest{
data("dat.bjelakovic") ## log odds ratio
## perform the penaliztion method by tuning tau
out21 <- meta.pen(y, s2, dat.bjelakovic)
## plot the loss function and candidate taus
plot(out21$tau.cand, out21$loss, xlab = NA, ylab = NA, 
  lwd=1.5, type = "l", cex.lab = 0.8, cex.axis = 0.8)
title(xlab = expression(paste(tau[t])),
  ylab = expression(paste("Loss function ", ~hat(L)(tau[t]))),
  cex.lab = 0.8, line = 2)
idx <- which(out21$loss == min(out21$loss))
abline(v = out21$tau.cand[idx], col = "gray", lwd = 1.5, lty = 2)

out22 <- meta.pen(y, s2, dat.bjelakovic, tuning.para = "lambda")

data("dat.carless") ## log odds ratio
## perform the penaliztion method by tuning tau
out31 <- meta.pen(y, s2, dat.carless)
## plot the loss function and candidate taus
plot(out31$tau.cand, out31$loss, xlab = NA, ylab = NA, 
  lwd=1.5, type = "l", cex.lab = 0.8, cex.axis = 0.8)
title(xlab = expression(paste(tau[t])),
  ylab = expression(paste("Loss function ", ~hat(L)(tau[t]))),
  cex.lab = 0.8, line = 2)
idx <- which(out31$loss == min(out31$loss))
abline(v = out31$tau.cand[idx], col = "gray", lwd = 1.5, lty = 2)

out32 <- meta.pen(y, s2, dat.carless, tuning.para = "lambda")

data("dat.adams") ## mean difference
out41 <- meta.pen(y, s2, dat.adams)
## plot the loss function and candidate taus
plot(out41$tau.cand, out41$loss, xlab = NA, ylab = NA, 
  lwd=1.5, type = "l", cex.lab = 0.8, cex.axis = 0.8)
title(xlab = expression(paste(tau[t])),
  ylab = expression(paste("Loss function ", ~hat(L)(tau[t]))),
  cex.lab = 0.8, line = 2)
idx <- which(out41$loss == min(out41$loss))
abline(v = out41$tau.cand[idx], col = "gray", lwd = 1.5, lty = 2)

out42 <- meta.pen(y, s2, dat.adams, tuning.para = "lambda")
}
}
\keyword{heterogeneity}