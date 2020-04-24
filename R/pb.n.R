pb.n <- function(y, s2 = NA, n = NA, n00, n01, n10, n11){
  n <- n00 + n01 + n10 + n11
  nSum0 <- n00 + n10
  nSum1 <- n01 + n11
  x.reg <- n
  y.reg <- y
  w.reg <- nSum0*nSum1/n
  if(all(abs(diff(x.reg[!is.na(x.reg)])) < 1e-5) | all(abs(diff(y.reg[!is.na(y.reg)])) < 1e-5)){
    out <- list(pval = 1, stat = 0, coef = c(0, 0))
  }else{
    out <- lm(y.reg ~ x.reg, weights = w.reg)
    out <- summary(out)$coefficients
    pval <- out["x.reg", "Pr(>|t|)"]
    coef <- as.numeric(out[,1])
    out <- list(pval = pval, stat = as.numeric(out["x.reg", "t value"]), coef = coef)
  }
  return(out)
}