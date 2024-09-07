meta.or.smd <- function(y1, sd1, n1, y0, sd0, n0, r1, m1, r0, m0, id.bin, data,
  n.adapt = 1000, n.chains = 3, n.burnin = 5000, n.iter = 20000, n.thin = 2,
  seed = 1234){

  if(missing(y1) | missing(y0)) stop("please specify sample means for the continuous outcome.")
  if(missing(sd1) | missing(sd0)) stop("please specify sample standard deviations for the continuous outcome.")
  if(missing(n1) | missing(n0)) stop("lease specify sample sizes for the continuous outcome.")
  if(missing(r1) | missing(r0)) stop("please specify event counts for the binary outcome.")
  if(missing(m1) | missing(m0)) stop("please specify sample sizes for the binary outcome.")
  if(!missing(data)){
    y1 <- eval(substitute(y1), data, parent.frame())
    sd1 <- eval(substitute(sd1), data, parent.frame())
    n1 <- eval(substitute(n1), data, parent.frame())
    y0 <- eval(substitute(y0), data, parent.frame())
    sd0 <- eval(substitute(sd0), data, parent.frame())
    n0 <- eval(substitute(n0), data, parent.frame())
    r1 <- eval(substitute(r1), data, parent.frame())
    m1 <- eval(substitute(m1), data, parent.frame())
    r0 <- eval(substitute(r0), data, parent.frame())
    m0 <- eval(substitute(m0), data, parent.frame())
    id.bin <- eval(substitute(id.bin), data, parent.frame())
  }

  ## Data
  jags.data <- list(
    M = length(which(data$id.bin == 1)), 
    m0 = data$m0[which(data$id.bin == 1)], 
    r0 = data$r0[which(data$id.bin == 1)], 
    m1 = data$m1[which(data$id.bin == 1)], 
    r1 = data$r1[which(data$id.bin == 1)],
    N = length(which(data$id.bin == 0)), 
    n0 = data$n0[which(data$id.bin == 0)], 
    y0 = data$y0[which(data$id.bin == 0)], 
    sd.sq0 = data$sd0[which(data$id.bin == 0)]^2, 
    n1 = data$n1[which(data$id.bin == 0)], 
    y1 = data$y1[which(data$id.bin == 0)], 
    sd.sq1 = data$sd1[which(data$id.bin == 0)]^2,
    idx.bin = which(data$id.bin == 1), 
    idx.cont = which(data$id.bin == 0))
  
  ## MCMC
  set.seed(seed)
  inits <- NULL
  rng.seeds <- sample(1000000, 3)
  for(i in 1:n.chains){
    inits[[i]] <- list(d = rnorm(1), tau = runif(1, 0, 2),
      .RNG.name = "base::Wichmann-Hill", .RNG.seed = sample(123456789, 1))
  }

  set.seed(seed)
  jags.m <- jags.model(file = textConnection(meta.or.smd.model()),
                       data = jags.data, inits = inits, n.chains = n.chains, 
                       n.adapt = n.adapt)
  update(jags.m, n.burnin)  ## burn_in
  params <- c("d", "tau", "theta")
  samps <- coda.samples(model = jags.m, variable.names = params,
    n.iter = n.iter, thin = n.thin)
  bayes.result <- summary(samps)  ## Bayesian summary
  
  ## Bayesian results
  bayes.smd.dt <- structure(
    list(median = bayes.result$quantiles[,"50%"], 
      lci = bayes.result$quantiles[,"2.5%"],
      uci = bayes.result$quantiles[,"97.5%"]), 
    .Names = c("median", "lci", "uci"),
    row.names = row.names(bayes.result$quantiles),
    class = "data.frame")
  
  return(bayes.smd.dt) # return the overall SMD, between-study SD, and individual studies' SMDs
}