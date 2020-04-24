pb.count <- function(y = NA, s2 = NA, n = NA, n00, n01, n10, n11){
  n <- n00 + n01 + n10 + n11
  n1Sum <- n10 + n11
  nSum1 <- n01 + n11
  or.mh <- sum(n00/n*n11)/sum(n01/n*n10)
  N <- length(n00)
  n11.e <- n11.v <- rep(NA, N)
  for(i in 1:N){
    obj <- hypergeometric(n1Sum[i], nSum1[i], n[i], or.mh)
    n11.e[i] <- obj$mean()
    n11.v[i] <- obj$var()
  }
  xx <- (n11 - n11.e)/sqrt(n11.v)
  yy <- 1/n11.v
  if(all(abs(diff(xx[!is.na(xx)])) < 1e-5) | all(abs(diff(yy[!is.na(yy)])) < 1e-5)){
    out <- list(pval = 1, stat = 0)
  }else{
    out <- cor.test(x = xx, y = yy, alternative = "two.sided", method = "kendall", exact = FALSE)
    out <- list(pval = out$p.value, stat = as.numeric(out$statistic))
  }
  return(out)
}