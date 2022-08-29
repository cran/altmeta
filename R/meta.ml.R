meta.ml <- function(y, s2, tol = 10^(-10)) {
  if(missing(y)) stop("please specify effect size.")
  if(missing(s2)) stop("please specify within-study variance.")
  if(length(y) != length(s2) | any(s2 < 0)) stop("error in the input data.")
  if (var(y) == 0) stop(return(0))
  
  mu.hat <- function(tau2) {
    sum(y/(s2 + tau2))/sum(1/(s2 + tau2))
  }
  mu.hat <- Vectorize(mu.hat)
  
  target.ml <- function(tau2) {
    mean(log(s2 + tau2) + (y - mu.hat(tau2))^2/(s2 + tau2))
  }
  target.ml <- Vectorize(target.ml)
  
  target1.ml <- function(tau2) {
    p1 <- mean(1/(s2 + tau2))
    p2 <- mean((y - mu.hat(tau2))^2/(s2 + tau2)^2)
    out <- p1 - p2
    return(out)
  }
  target1.ml <- Vectorize(target1.ml)
  
  tau2.ml <- optim(par = var(y), fn = target.ml, gr = target1.ml, method = "Brent", lower = 0, upper = 100 * 
                     var(y), control = list(reltol = tol))$par
  
  return(tau2.ml)
}
