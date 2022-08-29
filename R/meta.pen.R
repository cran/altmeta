meta.pen <- function(y, s2, data, tuning.para = "tau", upp = 1, n.cand = 100, tol=1e-10){
  if(missing(y)) stop("please specify effect size.")
  if(missing(s2)) stop("please specify within-study variance.")
  if(!missing(data)){
    y <- eval(substitute(y), data, parent.frame())
    s2 <- eval(substitute(s2), data, parent.frame())
  }
  if(length(y) != length(s2) | any(s2 < 0)) stop("error in the input data.")
  if (!is.element(tuning.para, c("tau", "lambda"))) stop("the tuning parameter must be specified as 'tau' or 'lambda'.")
  n <- length(y)
  w <- 1/s2
  y.bar <- sum(w*y)/sum(w)
  Q <- sum(w*(y - y.bar)^2)
  if(Q < n - 1) Q <- n - 1
  I2 <- (Q - (n - 1))/Q 
  
  tau2.re <- meta.ml(y, s2, tol) ## The ML estimate of the between-study variance, other methods can be used to calculate the estimate
  mu.fe <- y.bar
  se.fe <- sqrt(1/sum(w))
  w.re <- 1/(s2 + tau2.re)
  mu.re <- sum(w.re*y)/sum(w.re)
  se.re <- sqrt(1/sum(w.re))
  
  if(tuning.para == "tau"){
    tau.cand <- seq(from = 0, to = sqrt(upp*tau2.re), length.out = n.cand)
    tau2.cand <- tau.cand^2
    errs <- matrix(0, n, n.cand)
    for (i in 1:n) {
      y.train <- y[-i]
      s2.train <- s2[-i]
      tau2.re.train <- meta.ml(y.train, s2.train, tol)
      for (j in 1:n.cand) {
        tau2.temp <- tau2.cand[j]
        w.temp <- 1/(s2.train + tau2.temp)
        mu_i <- sum(w.temp*y.train)/sum(w.temp)
        var_i <- sum(w.temp^2*(s2.train + tau2.re.train))/(sum(w.temp))^2
        errs[i, j] <- (mu_i - y[i])^2/(var_i + s2[i] + tau2.re.train)
      }
    }
    loss <- sqrt(colMeans(errs))
    opt.idx <- which(loss == min(loss))[1] ## Use the first value to achieve minimization
    tau2.opt <- tau2.cand[opt.idx]
    tau.opt <- tau.cand[opt.idx]
    w.opt <- 1/(s2 + tau2.opt)
    mu.opt <- sum(w.opt*y)/sum(w.opt)
    var.opt <- sum(w.opt^2*(s2 + tau2.re))/(sum(w.opt))^2
    
    out <- NULL
    out$n.study <- n
    out$I2 <- I2
    out$tau2.re <- tau2.re
    out$mu.fe <- mu.fe
    out$se.fe <- se.fe
    out$mu.re <- mu.re
    out$se.re <- se.re
    out$loss <- loss
    out$tau.cand <- tau.cand
    out$tau.opt <- tau.opt ## The estimated between-study standard deviation by tuning tau
    out$mu.opt <- mu.opt  ## The estimated overall effect size by tuning tau
    out$se.opt <- sqrt(var.opt)  ## The standard error of the overall effect size estimate by tuning tau
    return(out)
  }
  
  if(tuning.para == "lambda"){
    lambda <- determine.lambda(y, s2, tau2.re, penalty = "tau2", n.lambda = n.cand, lambda.scale = "log", tol = tol, lam.c = upp)
    
    diff <- matrix(NA, n.cand, n)
    for (i in 1:n) {
      y_i <- y[-i]
      s2_i <- s2[-i]
      w_i <- 1/s2_i
      tau2.re_i <- meta.ml(y_i, s2_i, tol)
      out <- metapen.lambda(y_i, s2_i, penalty = "tau2", lambda = lambda, tol = tol)
      mu_i <- out[, "mu"]
      tau2_i <- out[, "tau2"]
      mu.hat.var <- lapply(X = 1:n.cand, FUN = function(lam) {
        sum((s2_i + tau2.re_i)/(s2_i + tau2_i[lam])^2)/(sum(1/(s2_i + tau2_i[lam])))^2
      })
      mu.hat.var <- unlist(mu.hat.var)
      diffi <- (y[i] - mu_i)^2/(s2[i] + tau2.re_i + mu.hat.var)
      diff[, i] <- diffi
    }

    full <- metapen.lambda(y, s2, penalty = "tau2", lambda = lambda, tol = tol)
    mu <- full[, "mu"]
    tau2 <- full[, "tau2"]
    tau <- sqrt(tau2)

    loss <- sqrt(apply(diff, 1, mean))
    opt.idx <- which(loss == min(loss))[1]
    tau2.opt <- tau2[opt.idx]
    tau.opt <- tau[opt.idx]
    w.opt <- 1/(s2 + tau2.opt)
    mu.opt <- sum(w.opt*y)/sum(w.opt)
    var.opt <- sum(w.opt^2*(s2 + tau2.re))/(sum(w.opt))^2

    out <- NULL
    out$n.study <- n
    out$I2 <- I2
    out$tau2.re <- tau2.re
    out$mu.fe <- mu.fe
    out$se.fe <- se.fe
    out$mu.re <- mu.re
    out$se.re <- se.re
    out$loss <- loss
    out$lambda.cand <- lambda
    out$tau.opt <- tau.opt ## The estimated between-study standard deviation by tuning lambda
    out$mu.opt <- mu.opt  ## The estimated overall effect size by tuning lambda
    out$se.opt <- sqrt(var.opt)  ## The standard error of the overall effect size estimate by tuning lambda
    return(out)
  }
}
