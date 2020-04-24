pb.reg <- function(y, s2, n = NA, n00 = NA, n01 = NA, n10 = NA, n11 = NA){
  x.reg <- sqrt(s2)
  y.reg <- y
  if(all(abs(diff(x.reg[!is.na(x.reg)])) < 1e-5) | all(abs(diff(y.reg[!is.na(y.reg)])) < 1e-5)){
    out <- list(pval = 1, stat = 0, coef = c(0, 0), std.res = y.reg/sqrt(s2))
  }else{
    w.reg <- 1/s2
    X <- cbind(1, x.reg)
    Y <- as.matrix(y.reg)
    W <- diag(w.reg)
    XWX <- t(X) %*% W %*% X
    if(as.numeric(rankMatrix(XWX)) < dim(XWX)[1]){
      out <- list(pval = 1, stat = 0, coef = c(0, 0), std.res = y.reg/sqrt(s2))
    }else{
      inv <- solve(XWX)
      coef <- inv %*% t(X) %*% W %*% Y
      var.coef <- inv
      pval <- as.numeric(2*pnorm(-abs(coef[2, 1])/sqrt(var.coef[2, 2])))
      res <- Y - X %*% coef
      std.res <- res/sqrt(s2)
      out <- list(pval = pval, stat = as.numeric(coef[2, 1]/sqrt(var.coef[2, 2])), coef = as.numeric(coef), std.res = std.res)
    }
  }
  return(out)
}