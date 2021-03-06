## JAGS model for RE NMA with evidence inconsistency
nma.re.ic <- function(o){
out <- "
model{
  for(i in 1:NS){
    delta[i,1] <- 0
    mu[i] ~ dnorm(0, 0.0001)  # vague priors for trial baselines
    for(k in 1:na[i]){
      r[i,k] ~ dbin(p[i,k], n[i,k])  # binomial likelihood
      logit(p[i,k]) <- mu[i] + delta[i,k]  # model
      rhat[i,k] <- p[i,k]*n[i,k] # expected counts
      dev[i,k] <- 2*(r[i,k]*(log(r[i,k]) - log(rhat[i,k])) +
        (n[i,k] - r[i,k])*(log(n[i,k] - r[i,k]) - log(n[i,k] - rhat[i,k])))
                                      # deviance contribution
    }
    resdev[i] <- sum(dev[i,1:na[i]]) # residual deviance for this trial
    for(k in 2:na[i]){
      delta[i,k] ~ dnorm(lor[t[i,1],t[i,k]], prec)  # LOR
    }
  }
  totresdev <- sum(resdev[])  # total residual deviance

  for(c in 1:(NT - 1)){
    for(k in (c + 1):NT){
      lor[c,k] ~ dnorm(0, 0.0001)
    }
  }

  prec <- 1/tau^2
  tau ~ dunif(0, 5)  #  prior for heterogeneity standard deviation
}"
return(out)
}