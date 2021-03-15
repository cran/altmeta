## JAGS model for FE NMA with evidence consistency
nma.fe.c <- function(o){
out <- "
model{
  for(i in 1:NS){
    mu[i] ~ dnorm(0, 0.0001)  # vague priors for trial baselines
    for(k in 1:na[i]){
      r[i,k] ~ dbin(p[i,t[i,k]], n[i,k])  # binomial likelihood
      logit(p[i,t[i,k]]) <- mu[i] + md[i,t[i,k]]  # model
      rhat[i,k] <- p[i,t[i,k]]*n[i,k] # expected counts
      dev[i,k] <- 2*(r[i,k]*(log(r[i,k]) - log(rhat[i,k])) +
        (n[i,k] - r[i,k])*(log(n[i,k] - r[i,k]) - log(n[i,k] - rhat[i,k])))
                                      # deviance contribution
    }
    resdev[i] <- sum(dev[i,1:na[i]]) # residual deviance for this trial
    md[i,t[i,1]] <- 0
    for(k in 2:na[i]){
      md[i,t[i,k]] <- d[t[i,k]] - d[t[i,1]]  # LOR
    }
  }
  totresdev <- sum(resdev[])  # total residual deviance

  d[1] <- 0
  for(k in 2:NT){
    d[k] ~ dnorm(0, 0.0001)
  }

  # pairwise ORs
  for(c in 1:(NT - 1)){
    for(k in (c + 1):NT){
      lor[c,k] <- d[k] - d[c]
    }
  }
}"
return(out)
}