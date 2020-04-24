pb.rank <- function(y, s2){
  theta.hat <- sum(y/s2)/sum(1/s2)
  s2.star <- s2 - 1/sum(1/s2)
  y.star <- (y - theta.hat)/sqrt(s2.star)
  if(all(abs(diff(y.star[!is.na(y.star)])) < 1e-5) | all(abs(diff(s2.star[!is.na(s2.star)])) < 1e-5)){
    out <- list(pval = 1, stat = 0)
  }else{
    out <- cor.test(x = y.star, y = s2, alternative = "two.sided", method = "kendall", exact = FALSE)
    out <- list(pval = out$p.value, stat = as.numeric(out$statistic))
  }
  return(out)
}