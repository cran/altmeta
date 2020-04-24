check.counts <- function(n00, n01, n10, n11){
  idx.double.zero <- which(n00 + n10 == 0 | n01 + n11 == 0)
  if(length(idx.double.zero) > 0){
    if(length(n00) - length(idx.double.zero) >= 5){
      n00 <- n00[-idx.double.zero]
      n01 <- n01[-idx.double.zero]
      n10 <- n10[-idx.double.zero]
      n11 <- n11[-idx.double.zero]
    }else{
      # avoid removing too many double-zero studies
      n00[idx.double.zero] <- n00[idx.double.zero] + 0.01
      n01[idx.double.zero] <- n01[idx.double.zero] + 0.01
      n10[idx.double.zero] <- n10[idx.double.zero] + 0.01
      n11[idx.double.zero] <- n11[idx.double.zero] + 0.01
    }
  }

  idx.zero <- which(n00 == 0 | n01 == 0 | n10 == 0 | n11 == 0)
  if(length(idx.zero) > 0){
    n00[idx.zero] <- n00[idx.zero] + 0.5
    n01[idx.zero] <- n01[idx.zero] + 0.5
    n10[idx.zero] <- n10[idx.zero] + 0.5
    n11[idx.zero] <- n11[idx.zero] + 0.5
  }
  out <- list(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
  return(out)
}