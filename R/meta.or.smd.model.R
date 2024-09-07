## JAGS model for the Bayesian model for combining
## standardized mean differences and odds ratios
meta.or.smd.model <- function(o){
  out <- "
    model{
      for(i in 1:M){
        r0[i] ~ dbinom(p0[i], m0[i])
        r1[i] ~ dbinom(p1[i], m1[i])
        probit(p0[i]) <- delta[i]
        probit(p1[i]) <- delta[i] - theta[idx.bin[i]]
        delta[i] ~ dnorm(0, 0.0001)
        theta[idx.bin[i]] ~ dnorm(d, inv.tausq)
      }

      for(i in 1:N){
        y0[i] ~ dnorm(mu0[i], inv.var0[i])
        y1[i] ~ dnorm(mu1[i], inv.var1[i])
        sd.sq0[i] ~ dgamma(alpha0[i], beta0[i])
        sd.sq1[i] ~ dgamma(alpha1[i], beta1[i])

        inv.var0[i] <- n0[i]/sigsq[i]
        alpha0[i] <- (n0[i]-1)/2
        beta0[i] <- (n0[i]-1)/(2*sigsq[i])
        inv.var1[i] <- n1[i]/sigsq[i]
        alpha1[i] <- (n1[i]-1)/2
        beta1[i] <- (n1[i]-1)/(2*sigsq[i])

        mu1[i] <- mu0[i] + theta[idx.cont[i]] * sqrt(sigsq[i])

        theta[idx.cont[i]] ~ dnorm(d, inv.tausq)
        mu0[i] ~ dnorm(0, 0.0001)
        inv.sigsq[i] ~ dgamma(0.001, 0.001)
        sigsq[i] <- 1/inv.sigsq[i]
      }

      d ~ dnorm(0, 0.0001)
      inv.tausq <- pow(tau, -2)
      tau ~ dunif(0, 2)
    }"
  return(out)
}