nma.predrank <- function(sid, tid, r, n, data, n.adapt = 1000, n.chains = 3, n.burnin = 2000,
                         n.iter = 5000, n.thin = 2, lowerbetter = TRUE, pred = TRUE,
                         pred.samples = FALSE, trace = FALSE){
  if(missing(sid)) stop("need to specify study id from 1 to NS for each study.")
  if(missing(tid)) stop("need to specify treatment id from 1 to NT for each arm in each study.")
  if(missing(r)) stop("need to specify numer of responses for each arm in each study.")
  if(missing(n)) stop("need to specify number of subjects for each arm in each study.")
  if(!missing(data)){
    sid <- eval(substitute(sid), data, parent.frame())
    tid <- eval(substitute(tid), data, parent.frame())
    r <- eval(substitute(r), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if((length(sid) != length(tid)) | (length(sid) != length(r)) | (length(sid) != length(n))){
    stop("the input data have different dimensions.")
  }
  if(max(sid) < 2) stop("less than two studies in the network meta-analysis.")
  
  BayesianNMAModel <- function(o){
    out <- "
    model{
    for(i in 1:NS){
    w[i,1] <- 0
    delta[i,t[i,1]] <- 0
    mu[i] ~ dnorm(0, 0.0001)  #vague priors for trial baselines
    for(k in 1:na[i]){
    r[i,k] ~ dbin(p[i,t[i,k]], n[i,k])  #binomial likelihood
    logit(p[i,t[i,k]]) <- mu[i] + delta[i,t[i,k]]  #model
    }
    for(k in 2:na[i]){
    # trial-specific log OR
    delta[i,t[i,k]] ~ dnorm(md[i,t[i,k]], taud[i,t[i,k]])
    md[i,t[i,k]] <- d[t[i,k]] - d[t[i,1]] + sw[i,k]	#mean of LOR
    taud[i,t[i,k]] <- tau*2*(k - 1)/k  #precision of LOR
    # adjustment of multi-arm trials
    w[i,k] <- delta[i,t[i,k]] - d[t[i,k]] + d[t[i,1]]
    sw[i,k] <- sum(w[i,1:(k-1)])/(k - 1)
    }
    }
    
    d[1] <- 0
    for(k in 2:NT){
    d[k] ~ dnorm(0, 0.0001)
    }
    
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 5)  #heterogeneity standard deviation
    
    # pairwise ORs
    for(h in 1:(NT - 1)){
    for(k in (h + 1):NT){
    lor[h,k] <- d[k] - d[h]
    }
    }
    
    rank <- rank(d)
    }"
return(out)
  }
  
  dat <- data.frame(sid = sid, tid = tid, r = r, n = n)
  NS <- max(dat$sid)
  NT <- max(dat$tid) 
  na <- as.numeric(table(dat$sid)) 
  r <- n <- t <- matrix(NA, nrow = NS, ncol = max(na))
  for(i in 1:NS){
    r[i, 1:na[i]] <- dat$r[dat$sid == i]
    n[i, 1:na[i]] <- dat$n[dat$sid == i]
    t[i, 1:na[i]] <- dat$tid[dat$sid == i]
  }
  
  jags.dat <- list(NS = NS, NT = NT, na = na, r = r, n = n, t = t)
  jags.inits <- list(
    list(mu = rep(0, NS), d = c(NA, rep(0, NT - 1)), sigma = 0.5),
    list(mu = rep(0.5, NS), d = c(NA, rep(-0.5, NT - 1)), sigma = 0.3),
    list(mu = rep(-0.5, NS), d = c(NA, rep(0.5, NT - 1)), sigma = 0.8))
  jags.m <- jags.model(file = textConnection(BayesianNMAModel()),
                       data = jags.dat, inits = jags.inits,
                       n.chains = n.chains, n.adapt = n.adapt)
  update(jags.m, n.iter = n.burnin)
  params <- c("d","sigma")
  coda <- coda.samples(model = jags.m, variable.names = params,
                       n.iter = n.iter, thin = n.thin)
  
  all.post <- NULL
  for(i in 1:n.chains){
    all.post <- rbind(all.post, coda[[i]])
  }
  all.post <- t(all.post)
  
  iter.post <- dim(all.post)[2]
  d.post <- all.post[paste0("d[", 1:NT, "]"),]
  dim <- ncol(d.post)
  all.trts <- 1:NT
  
  P <- rep(list(NULL), NT)
  for(k in 1:NT){
    x <- (matrix(rep(d.post[k,], NT - 1), byrow = TRUE, nrow = NT - 1) 
          - d.post[-k,])
    x <- t(x)
    y <- matrix(NA, nrow = dim, ncol = NT-1)
    yavg <- matrix(NA, nrow = dim, ncol = 1)
    for(p in 1:dim){
      for(q in 1:(NT-1))
        if(!lowerbetter){
          if (x[p,q] > 0) {y[p,q] = 1} 
          else(y[p,q] = 0)
        }else {
          if (x[p,q] < 0) {y[p,q] = 1} 
          else(y[p,q] = 0)
        }
      yavg[p] = mean(y[p,])
    }
    P[[k]] <- yavg
  }
  
  P.score <-  matrix(NA, nrow = NT, ncol = 4)
  colnames(P.score) <-
    c("Mean", "Median", "95% CrI LB", "95% CrI UB")
  for(k in 1:NT){
    P.score[k,1] <- mean(P[[k]])
    P.score[k, 2:4] <- as.numeric(quantile(P[[k]],
                                           probs = c(0.5, 0.025, 0.975)))
  }
  
  if(pred){
    if(lowerbetter) d.post <- -d.post
    sigma.post <- all.post["sigma",]
    P.pred <- rep(list(NULL), NT)
    for(k in 1:NT){
      x <-
        (matrix(rep(d.post[k,], NT - 1), byrow = TRUE, nrow = NT - 1) -
           d.post[-k,])/
        matrix(rep(sigma.post, NT - 1), byrow = TRUE, nrow = NT - 1)
      x <- t(x)
      out <- pnorm(x, mean = rep(0, NT - 1), sd = 1)
      yavg <- matrix(NA, nrow = dim, ncol = 1)
      for(p in 1:dim){
        yavg[p] = mean(out[p,])
      }
      P.pred[[k]] <- yavg
    }
    
    P.score.pred <- matrix(NA, nrow = NT, ncol = 4)
    colnames(P.score.pred) <-
      c("Mean", "Median", "95% CrI LB", "95% CrI UB")
    for(k in 1:NT){
      P.score.pred[k,1] <- mean(P.pred[[k]])
      P.score.pred[k, 2:4] <- as.numeric(quantile(P.pred[[k]],
                                                  probs = c(0.5, 0.025, 0.975)))
    }
    
    out <- list(P.score = P.score, P.score.pred = P.score.pred)
    if(pred.samples) out$P.pred = P.pred
  }else{
    out <- list(P.score = P.score)
  }
  
  if(trace) out$coda <- coda
  
  class(out) <- "nma.predrank"
  return(out)
}