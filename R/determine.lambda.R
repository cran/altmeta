determine.lambda <- function(y, s2, tau2.re, penalty = "tau2", n.lambda = 100, lambda.scale = "log",
                             tol = 10^(-10), lam.c = 1.2) {
  if(missing(y)) stop("please specify effect size.")
  if(missing(s2)) stop("please specify within-study variance.")
  if(length(y) != length(s2) | any(s2 < 0)) stop("error in the input data.")
  if(missing(tau2.re)) {tau2.re <- meta.ml(y, s2, tol)}

  mu.hat <- function(tau2) {
    sum(y/(s2 + tau2))/sum(1/(s2 + tau2))
  }
  mu.hat <- Vectorize(mu.hat)

  if (penalty == "tau2") {
    fcn <- function(tau2) {
      sum((y - mu.hat(tau2))^2/(s2 + tau2)^2) - sum(1/(s2 + tau2))
    }
  }
  if (penalty == "tau") {
    fcn <- function(tau2) {
      (sum((y - mu.hat(tau2))^2/(s2 + tau2)^2) - sum(1/(s2 + tau2))) * (2 * sqrt(tau2))
    }
  }
  fcn <- Vectorize(fcn)
  lam.max <- optimize(f = fcn, lower = 0, upper = tau2.re*1.1, maximum = TRUE)$objective

  if (lambda.scale == "linear") lam.max <- max(c(0, lam.max))*lam.c
  if (lambda.scale == "log") lam.max <- exp(log(max(c(0, lam.max)) + 1) + log(lam.c)) - lam.c

  if (lambda.scale == "linear") lambda <- seq(from = 0, to = lam.max, length.out = n.lambda)
  if (lambda.scale == "log") lambda <- exp(seq(from = 0, to = log(lam.max + 1), length.out = n.lambda)) - 1

  return(lambda)
}
