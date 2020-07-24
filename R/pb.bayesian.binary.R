pb.bayesian.binary <- function(n00, n01, n10, n11, p01 = NULL, p11 = NULL, data, sig.level = 0.1, 
                               method = "bay", het = "mul", sd.prior = "unif", n.adapt = 1000, 
                               n.chains = 3, n.burnin = 5000, n.iter = 10000, thin = 2, upp.het = 2, 
                               phi = 0.5, coda = FALSE, traceplot = FALSE, seed = 1234){
  if(method != "bay" & method != "reg.bay" & method != "smoothed.bay") 
    stop("method must be bay, reg.bay or smoothed.bay")
  if(missing(n01) | missing(n11)) stop("the counts of events are required.")
  if(missing(n00) | missing(n10)) stop("the counts of non-events are required.")
  if(!missing(data)){
    n00 <- eval(substitute(n00), data, parent.frame())
    n01 <- eval(substitute(n01), data, parent.frame())
    n10 <- eval(substitute(n10), data, parent.frame())
    n11 <- eval(substitute(n11), data, parent.frame())
    if(!is.null(p01)) p01 <- eval(substitute(p01), data, parent.frame())
    if(!is.null(p11)) p01 <- eval(substitute(p11), data, parent.frame())
  }
  if(!is.null(p01) & !is.null(p11)){
    if(p01 > 1 | p11 > 1 | any(n00 < 0) | any(n01 < 0) | any(n10 < 0) | any(n11 < 0)) 
      stop("the event rates are invalid.")}
  
  n0 <- n00 + n01
  n1 <- n10 + n11
  n0_star <- n0
  n1_star <- n1
  n00_star <- n00
  n01_star <- n01
  n10_star <- n10
  n11_star <- n11
  identify.zero <- which(n00 == 0 | n01 == 0 | n10 == 0 | n11 == 0)
  if (length(identify.zero)>0){
    n00_star[identify.zero] <- n00_star[identify.zero] + 0.5
    n01_star[identify.zero] <- n01_star[identify.zero] + 0.5
    n10_star[identify.zero] <- n10_star[identify.zero] + 0.5
    n11_star[identify.zero] <- n11_star[identify.zero] + 0.5
    n0_star[identify.zero] <- n0_star[identify.zero] + 1
    n1_star[identify.zero] <- n1_star[identify.zero] + 1
    y <- log(n11_star/n10_star) - log(n01_star/n00_star)
    s2 <- 1/n11_star + 1/n10_star + 1/n01_star + 1/n00_star
    p01.sm <- mean(n01_star/n0_star)
    p11.sm <- mean(n11_star/n1_star)
  }else{
    y <- log(n11/n10) - log(n01/n00)
    s2 <- 1/n11 + 1/n10 + 1/n01 + 1/n00
    p01.sm <- mean(n01/n0)
    p11.sm <- mean(n11/n1)
  }
  s2.smoothed <- 1/(n0*p01.sm*(1 - p01.sm)) + 1/(n1*p11.sm*(1 - p11.sm))
  
  out <- NULL
  if(is.element("bay", method)){
    set.seed(seed)
    out <- pb.bay(y = y, s2 = s2, sig.level = sig.level, n00 = n00, n01 = n01, n10 = n10, n11 = n11, 
                      het = het, sd.prior = sd.prior, n.adapt = n.adapt, n.chains = n.chains, 
                      n.burnin = n.burnin, n.iter = n.iter, thin = thin, upp.het = upp.het, 
                      phi = phi, coda = coda, traceplot = traceplot)
  }
  
  if(is.element("reg.bay", method)){
    if(!is.null(p01) & !is.null(p11)){
      s2.true <- 1/(n0*p01*(1 - p01)) + 1/(n1*p11*(1 - p11))
      set.seed(seed)
      out <- pb.reg.bay(y = y, s2 = s2.true, sig.level = sig.level, n00 = n00, n01 = n01, n10 = n10, n11 = n11, 
                        het = het, sd.prior = sd.prior, n.adapt = n.adapt, n.chains = n.chains, 
                        n.burnin = n.burnin, n.iter = n.iter, thin = thin, upp.het = upp.het, 
                        phi = phi, coda = coda, traceplot = traceplot)
    }else{
      set.seed(seed)
      out <- pb.reg.bay(y = y, s2 = s2, sig.level = sig.level, n00 = n00, n01 = n01, n10 = n10, n11 = n11, 
                        het = het, sd.prior = sd.prior, n.adapt = n.adapt, n.chains = n.chains, 
                        n.burnin = n.burnin, n.iter = n.iter, thin = thin, upp.het = upp.het, 
                        phi = phi, coda = coda, traceplot = traceplot)
    }
  }
  
  if(is.element("smoothed.bay", method)){
    set.seed(seed)
    out.temp <- pb.reg.bay(y = y, s2 = s2.smoothed, sig.level = sig.level, n00 = n00, n01 = n01, n10 = n10, n11 = n11, 
                           het = het, sd.prior = sd.prior, n.adapt = n.adapt, n.chains = n.chains, 
                           n.burnin = n.burnin, n.iter = n.iter, thin = thin, upp.het = upp.het, 
                           phi = phi, coda = coda, traceplot = traceplot)
    out$est.smoothed.bay <- out.temp$est.reg.bay
    out$ci.smoothed.bay <- out.temp$ci.reg.bay
    out$samps.smoothed.bay <- out.temp$samp.reg.bay
  }
  
  return(out)
}