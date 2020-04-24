pb.reg.het <- function(y, s2, n = NA, n00 = NA, n01 = NA, n10 = NA, n11 = NA){
  k <- length(y)
  coef <- pb.reg(y, s2)$coef
  x.reg <- sqrt(s2)
  y.reg <- y
  w <- 1/s2
  Q <- sum(w*(y.reg - coef[1] - coef[2]*x.reg)^2)
  if(abs(sum(w)*sum(w*x.reg^2) - (sum(w*x.reg))^2) < 1e-5){
    F <- sum(w) - (sum(w^2)*sum(w*x.reg^2) - 2*sum(w^2*x.reg)*sum(w*x.reg) + sum(w)*sum(w^2*x.reg^2))*1e5
  }else{
    F <- sum(w) - (sum(w^2)*sum(w*x.reg^2) - 2*sum(w^2*x.reg)*sum(w*x.reg) + sum(w)*sum(w^2*x.reg^2))/(sum(w)*sum(w*x.reg^2) - (sum(w*x.reg))^2)
  }
  tau2.hat <- (Q - (k - 2))/F
  tau2.hat <- max(c(0, tau2.hat))

  x.reg <- sqrt(s2)
  y.reg <- y
  if(all(abs(diff(x.reg[!is.na(x.reg)])) < 1e-5) | all(abs(diff(y.reg[!is.na(y.reg)])) < 1e-5)){
    out <- list(pval = 1, stat = 0, coef = c(0, 0), std.res = y.reg/sqrt(s2 + tau2.hat))
  }else{
    w.reg <- 1/(s2 + tau2.hat)
    X <- cbind(1, x.reg)
    Y <- as.matrix(y.reg)
    W <- diag(w.reg)
    XWX <- t(X) %*% W %*% X
    if(as.numeric(rankMatrix(XWX)) < dim(XWX)[1]){
      out <- list(pval = 1, stat = 0, coef = c(0, 0), std.res = y.reg/sqrt(s2 + tau2.hat))
    }else{
      inv <- solve(XWX)
      coef <- inv %*% t(X) %*% W %*% Y
      var.coef <- inv
      pval <- as.numeric(2*pnorm(-abs(coef[2, 1])/sqrt(var.coef[2, 2])))
      res <- Y - X %*% coef
      std.res <- res/sqrt(s2 + tau2.hat)
      out <- list(pval = pval, stat = as.numeric(coef[2, 1]/sqrt(var.coef[2, 2])), coef = as.numeric(coef), std.res = std.res, tau2.hat = tau2.hat)
    }
  }
  return(out)
}