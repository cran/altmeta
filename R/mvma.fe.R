mvma.fe <- function(ys, covs, tol = 1E-10){
  if(missing(ys)) stop("the argument ys is missing.")
  if(missing(covs)) stop("the argument covs is missing.")
  ys <- as.matrix(ys)
  n <- dim(ys)[1]
  p <- dim(ys)[2]
  if(p < 2) stop("please directly use the univariate model.")

  idx.na <- numeric(0)
  for(i in 1:n){
    if(all(is.na(ys[i,]))) idx.na <- c(idx.na, i)
  }
  if(length(idx.na) > 0){
    ys <- ys[-idx.na,]
    covs <- covs[-idx.na]
    n <- dim(ys)[1]
  }

  idx.obs <- vector("list", n)
  X <- vector("list", n)
  ys.tilde <- vector("list", n)
  covs.tilde <- vector("list", n)
  for(i in 1:n){
    idx.obs[[i]] <- which(!is.na(ys[i,]))
    X[[i]] <- diag(p)[idx.obs[[i]],]
    if(length(idx.obs[[i]]) == 1) X[[i]] <- matrix(X[[i]], nrow = 1, ncol = p)
    ys.tilde[[i]] <- ys[i, idx.obs[[i]]]
    covs.tilde[[i]] <- as.matrix(covs[[i]][idx.obs[[i]], idx.obs[[i]]])
  }

  target <- function(x){
    mu <- x[1:p]
    Tau <- matrix(0, nrow = p, ncol = p)

    out1 <- 0
    for(i in 1:n){
      cov.temp <- covs.tilde[[i]] + Tau[idx.obs[[i]], idx.obs[[i]]]
      y.temp <- ys.tilde[[i]]
      y_mu <- y.temp - mu[idx.obs[[i]]]
      y_mu <- as.matrix(y_mu)
      logdet <- determinant(as.matrix(cov.temp), logarithm = TRUE)
      if(logdet$sign <= 0) logdet <- -Inf else logdet <- logdet$modulus
      temp <- logdet + as.numeric(t(y_mu)%*%solve(cov.temp)%*%y_mu)
      out1 <- out1 + temp
    }
    out <- as.numeric(out1)
    return(out)
  }

  inits <- rep(0, p)
  low <- rep(-Inf, p)
  upp <- rep(Inf, p)
  optim.out <- optim(par = inits, fn = target, lower = low, upper = upp, method = "L-BFGS-B", control = list(factr = tol))
  pars <- optim.out$par

  mu.est <- pars[1:p]
  Tau.est <- matrix(0, nrow = p, ncol = p)

  temp <- matrix(0, p, p)
  for(i in 1:n){
    cov.temp <- covs.tilde[[i]] + Tau.est[idx.obs[[i]], idx.obs[[i]]]
    if(length(idx.obs[[i]]) == 1){
      cov.temp <- as.numeric(cov.temp)
      temp <- temp + t(X[[i]])%*%X[[i]]/cov.temp
    }else{
      temp <- temp + t(X[[i]])%*%solve(cov.temp)%*%X[[i]]
    }
  }
  mu.cov <- solve(temp)

  out <- list(mu.est = mu.est, mu.cov = mu.cov, method = "Fixed-effects model")
  return(out)
}