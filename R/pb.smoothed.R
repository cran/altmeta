pb.smoothed <- function(y, s2 = NA, n = NA, n00, n01, n10, n11){
  p01 <- mean(n01/(n00 + n01))
  p00 <- 1 - p01
  p11 <- mean(n11/(n10 + n11))
  p10 <- 1 - p11
  s2.smoothed <- 1/((n00 + n01)*p00) + 1/((n00 + n01)*p01) + 1/((n10 + n11)*p10) + 1/((n10 + n11)*p11)
  out <- pb.reg(y = y, s2 = s2.smoothed)
  return(out)
}