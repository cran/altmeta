metapen.lambda <- function(y, s2, penalty = "tau2", lambda, tau2.re, tol = 10^(-10)) {
  if(missing(y)) stop("please specify effect size.")
  if(missing(s2)) stop("please specify within-study variance.")
  if(missing(lambda)) stop("please specify candidates of lambda to optimize.")
  if(length(y) != length(s2) | any(s2 < 0)) stop("error in the input data.")
  if(missing(tau2.re)) {tau2.re <- meta.ml(y, s2, tol)}

  n <- length(y)

  if (penalty == "tau2") {
    pen <- function(tau2) {
      tau2
    }
    pen1 <- function(tau2) {
      1
    }
  } else {
    pen <- function(tau2) {
      sqrt(tau2)
    }
    pen1 <- function(tau2) {
      1/(2 * sqrt(tau2))
    }
  }
  pen <- Vectorize(pen)
  pen1 <- Vectorize(pen1)

  mu.hat <- function(tau2) {
    sum(y/(s2 + tau2))/sum(1/(s2 + tau2))
  }
  mu.hat <- Vectorize(mu.hat)

  tau2.hat <- function(lambda) {
    target <- function(tau2) {
      mean(log(s2 + tau2) + (y - mu.hat(tau2))^2/(s2 + tau2)) + lambda * pen(tau2)/n
    }
    target <- Vectorize(target)

    target1 <- function(tau2) {
      p1 <- mean(1/(s2 + tau2))
      p2 <- mean((y - mu.hat(tau2))^2/(s2 + tau2)^2)
      out <- p1 - p2 + lambda * pen1(tau2)/n
      return(out)
    }

    out <- optim(par = tau2.re, fn = target, gr = target1, method = "Brent", lower = 0, upper = tau2.re *
                   1.1, control = list(reltol = tol))$par
    return(out)
  }
  tau2.hat <- Vectorize(tau2.hat)

  out.tau2 <- tau2.hat(lambda)
  out.mu <- mu.hat(out.tau2)
  out <- cbind(lambda = lambda, mu = out.mu, tau2 = out.tau2)
  rownames(out) <- rep("", length(lambda))
  return(out)
}
