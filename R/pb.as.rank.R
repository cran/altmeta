pb.as.rank <- function(y = NA, s2 = NA, n = NA, n00, n01, n10, n11){
  delta <- asin(sqrt(n11/(n10 + n11))) - asin(sqrt(n01/(n00 + n01)))
  gamma <- 1/(4*(n10 + n11)) + 1/(4*(n00 + n01))
  out <- pb.rank(y = delta, s2 = gamma)
  return(out)
}