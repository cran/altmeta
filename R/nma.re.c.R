## JAGS model for RE NMA with evidence consistency
nma.re.c <- function(o){
out <- "
model{
  for(i in 1:NS){
    w[i,1] <- 0
    delta[i,t[i,1]] <- 0
    mu[i] ~ dnorm(0, 0.0001)  # vague priors for trial baselines
    for(k in 1:na[i]){
      r[i,k] ~ dbin(p[i,t[i,k]], n[i,k])  # binomial likelihood
      logit(p[i,t[i,k]]) <- mu[i] + delta[i,t[i,k]]  # model
      rhat[i,k] <- p[i,t[i,k]]*n[i,k] # expected counts
      dev[i,k] <- 2*(r[i,k]*(log(r[i,k]) - log(rhat[i,k])) +
        (n[i,k] - r[i,k])*(log(n[i,k] - r[i,k]) - log(n[i,k] - rhat[i,k])))
                                      # deviance contribution
    }
    resdev[i] <- sum(dev[i,1:na[i]]) # residual deviance for this trial
    for(k in 2:na[i]){
      delta[i,t[i,k]] ~ dnorm(md[i,t[i,k]], taud[i,t[i,k]])  # LOR
      md[i,t[i,k]] <- d[t[i,k]] - d[t[i,1]] + sw[i,k]  # mean of LOR
      taud[i,t[i,k]] <- prec*2*(k - 1)/k  # precision of LOR
      w[i,k] <- delta[i,t[i,k]] - d[t[i,k]] + d[t[i,1]]  # multi-arm RCTs
      sw[i,k] <- sum(w[i,1:(k-1)])/(k - 1)
    }
  }
  totresdev <- sum(resdev[])  # total residual deviance

  d[1] <- 0
  for(k in 2:NT){
    d[k] ~ dnorm(0, 0.0001)
  }

  prec <- 1/tau^2
  tau ~ dunif(0, 5)  #  prior for heterogeneity standard deviation

  # pairwise ORs
  for(c in 1:(NT - 1)){
    for(k in (c + 1):NT){
      lor[c,k] <- d[k] - d[c]
    }
  }
}"
return(out)
}