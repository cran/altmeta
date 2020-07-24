pb.reg.bay <- function(y, s2 = NA, sig.level = 0.1, n00, n01, n10, n11, het = "mul", 
                       sd.prior = "unif", n.adapt = 1000, n.chains = 3, 
                       n.burnin = 5000, n.iter = 10000, thin = 2, upp.het = 2, 
                       phi = 0.5, coda = FALSE, traceplot = FALSE){
  out <- NULL
  if(is.element("mul", het)){
    if(is.element("unif", sd.prior)){
      modelstring <- function(o){
        out <- "
        model{
        for(i in 1:N){
        y[i] ~ dnorm(alpha + beta*se[i], prec[i])
        prec[i] <- pow(kappa*se[i], -2)
        }
        alpha ~ dnorm(0, 0.0001)
        beta ~ dnorm(0, 0.0001)
        kappa ~ dunif(0, upp.kappa)
        }"
  return(out)
      }
      
      jags.dat <- list(N = length(y), y = y, se = sqrt(s2), upp.kappa = upp.het)
      inits <- NULL
      rng.seeds <- sample(1000000, n.chains)
      for(i in 1:n.chains){
        inits[[i]] <- list(alpha = rnorm(1), beta = rnorm(1),
                           kappa = runif(1, 0.1, 1.9), 
                           .RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[i])
      }
      
    }
    if(is.element("hn", sd.prior)){
      modelstring <- function(o){
        out <- "
        model{
        for(i in 1:N){
        y[i] ~ dnorm(alpha + beta*se[i], prec[i])
        prec[i] <- pow(kappa*se[i], -2)
        }
        alpha ~ dnorm(0, 0.0001)
        beta ~ dnorm(0, 0.0001)
        kappa ~ dnorm(0, inv.phi2)I(0,)
        }"
  return(out)
      }
      
      jags.dat <- list(N = length(y), y = y, se = sqrt(s2), inv.phi2 = phi^(-2))
      inits <- NULL
      rng.seeds <- sample(1000000, n.chains)
      for(i in 1:n.chains){
        inits[[i]] <- list(alpha = rnorm(1), beta = rnorm(1),
                           kappa = runif(1, 0.1, 1.9), 
                           .RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[i])
      }
      
    }
    
    }
  
  if(is.element("add", het)){
    if(is.element("unif", sd.prior)){
      modelstring <- function(o){
        out <- "
        model{
        for(i in 1:N){
        y[i] ~ dnorm(alpha + beta*se[i], prec[i])
        prec[i] <- 1/(pow(gamma, 2) + pow(se[i], 2))
        }
        alpha ~ dnorm(0, 0.0001)
        beta ~ dnorm(0, 0.0001)
        gamma ~ dunif(0, upp.gamma)
        }"
      return(out)
      }
      
      jags.dat <- list(N = length(y), y = y, se = sqrt(s2), upp.gamma = upp.het)
      inits <- NULL
      rng.seeds <- sample(1000000, n.chains)
      for(i in 1:n.chains){
        inits[[i]] <- list(alpha = rnorm(1), beta = rnorm(1), 
                           gamma = runif(1, 0.1, 1.9), 
                           .RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[i])
      }
      
    }
    if(is.element("hn", sd.prior)){
      modelstring <- function(o){
        out <- "
        model{
        for(i in 1:N){
        y[i] ~ dnorm(alpha + beta*se[i], prec[i])
        prec[i] <- 1/(pow(gamma, 2) + pow(se[i], 2))
        }
        alpha ~ dnorm(0, 0.0001)
        beta ~ dnorm(0, 0.0001)
        gamma ~ dnorm(0, inv.phi2)I(0,)
        }"
      return(out)
      }
      
      jags.dat <- list(N = length(y), y = y, se = sqrt(s2), inv.phi2 = phi^(-2))
      inits <- NULL
      rng.seeds <- sample(1000000, n.chains)
      for(i in 1:n.chains){
        inits[[i]] <- list(alpha = rnorm(1), beta = rnorm(1), 
                           gamma = runif(1, 0.1, 1.9), 
                           .RNG.name = "base::Wichmann-Hill", .RNG.seed = rng.seeds[i])
      }
    }
    
    
    }
  
  jags.m <- jags.model(file = textConnection(modelstring()),
                       data = jags.dat, inits = inits, n.chains = n.chains, n.adapt = n.adapt)
  update(jags.m, n.iter = n.burnin)
  params <- c("beta")
  samps <- coda.samples(model = jags.m, variable.names = params,
                        n.iter = n.iter, thin = thin)
  quants <- quantile(unlist(samps),
                     probs = c(sig.level/2, 1 - sig.level/2, 0.5))
  
  if(traceplot){
    par(mfrow = c(n.chains,1))
    for(i in 1:n.chains){
      temp <- as.vector(samps[[i]])
      tp <- plot(temp, type = "l", col = "red", ylab = paste("beta[", i, "]", sep = ""), 
                 xlab = "Iterations", main = paste("Chain",i))
    }  
    print(tp)
  }
  
  out$est.reg.bay <- as.numeric(quants[3])
  out$ci.reg.bay <- quants[c(1, 2)]
  
  temp <- name <- NULL
  if(coda){
    for(i in 1:n.chains){
      temp <- cbind(temp, as.vector(samps[[i]]))
      name <- c(name, paste("beta[", i, "]", sep = ""))
    }
    beta <- data.frame(temp)
    names(beta) <- name
    out$samps.reg.bay <- beta
  }
  return(out)
  }
