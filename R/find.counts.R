find.counts <- function(n0., n1., lor, lor.var, p0.ori){
  r <- exp(lor)
  s <- lor.var
  a <- (1 - r)^2 + n1.*r*s
  b <- -2*(1 - r) - n1.*r*s
  c <- 1 + n1./n0.*r
  D <- b^2 - 4*a*c
  if(D < 0) p0 <- c(0, 1)
  if(D == 0){
    p0 <- -b/(2*a)
    if(p0 < 0) p0 <- 0
    if(p0 > 1) p0 <- 1
  }
  if(D > 0){
    p0.sol1 <- (-b - sqrt(D))/(2*a)
    p0.sol2 <- (-b + sqrt(D))/(2*a)
    p0 <- c(p0.sol1, p0.sol2)
    p0[p0 < 0] <- 0
    p0[p0 > 1] <- 1
  }
  if(length(p0) > 1){
    idx <- which(p0 - p0.ori == min(p0 - p0.ori))
    p0 <- p0[idx]
    p0 <- p0[1]
  }
  p1 <- r*p0/(1 - p0 + r*p0)
  n01 <- n0.*p0
  n00 <- n0.*(1 - p0)
  n11 <- n1.*p1
  n10 <- n1.*(1 - p1)
  out <- list(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
  return(out)
}