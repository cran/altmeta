pb.inv.sqrt.n <- function(y, s2 = NA, n, n00 = NA, n01 = NA, n10 = NA, n11 = NA){
  x.reg <- 1/sqrt(n)
  y.reg <- y
  w.reg <- n
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