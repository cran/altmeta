pb.skew <- function(y, s2, n = NA, n00 = NA, n01 = NA, n10 = NA, n11 = NA){
  std.res <- pb.reg(y, s2)$std.res
  cm2 <- var(std.res)
  cm3 <- mean((std.res - mean(std.res))^3)
  skewness <- cm3/(cm2^(1.5))
  pval <- 2*pnorm(-sqrt(length(y)/6)*abs(skewness))
  out <- list(pval = as.numeric(pval), stat = as.numeric(skewness))
  return(out)
}