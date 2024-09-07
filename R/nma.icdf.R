## Calculating ICDF: la, Lu--Ades; fe, fixed-effects; re, random-effects
nma.icdf <- function(sid, tid, r, n, data, type = c("la", "fe", "re"),
  n.adapt = 1000, n.chains = 3, n.burnin = 5000, n.iter = 20000, n.thin = 2,
  traceplot = FALSE, nma.name = NULL, seed = 1234){
  if(missing(sid)) stop("please specify study ID.")
  if(missing(tid)) stop("please specify treatment ID.")
  if(missing(r)) stop("please specify event count.")
  if(missing(n)) stop("please specify sample size.")
  if(!missing(data)){
    sid <- eval(substitute(sid), data, parent.frame())
    tid <- eval(substitute(tid), data, parent.frame())
    r <- eval(substitute(r), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if(all(!is.element(type, c("la", "fe", "re")))) stop("the type of ICDF is wrongly specified.")
  if(any(!is.element(type, c("la", "fe", "re")))){
    type <- type[is.element(type, c("la", "fe", "re"))]
  }

  n.trt <- length(unique(data$tid))
  study.idx <- unique(data$sid)
  n.study <- length(study.idx)
  out <- NULL
  out$nstudy.trtarm <- table(table(data$sid))
  out$nstudy.multi <- sum(table(data$sid) > 2)
  multi.trtarm <- NULL
  multi.idx <- which(table(data$sid) > 2)
  if(out$nstudy.multi > 0){
    for(i in 1:out$nstudy.multi){
      multi.trtarm[[i]] <- sort(data$tid[data$sid == multi.idx[i]])
    }
  }
  if(is.null(multi.trtarm)){
    multi.trtarm <- NA
  }
  out$multi.trtarm <- multi.trtarm

  if(is.element("la", type)){
    if(out$nstudy.multi > 0){
      out$icdf.la <- NA
    }else{
      T <- 0
      for(h in 1:(n.trt - 1)){
        for(k in (h + 1):n.trt){
          T.temp <- 0
          for(i in 1:n.study){
            dat.temp <- data[data$sid == i,]
            if(dim(dat.temp)[1] == 2){
              trt1.temp <- min(dat.temp$tid)
              trt2.temp <- max(dat.temp$tid)
              if(trt1.temp == h & trt2.temp == k){
                T.temp <- 1
              }
            }
          }
          if(T.temp == 1) T <- T + 1
        }
      }
      out$icdf.la <- T - n.trt + 1
    }
  }

  out <- c(out, list(icdf.fe = NA, icdf.re = NA))

  if(is.element("fe", type) | is.element("re", type)){
    na <- as.numeric(table(data$sid))
    t <- r <- n <- matrix(NA, n.study, max(na))
    for(i in 1:n.study){
      dat.temp <- data[data$sid == study.idx[i],]
      ord <- order(dat.temp$tid)
      dat.temp <- dat.temp[ord,]
      t[i, 1:na[i]] <- dat.temp$tid
      r[i, 1:na[i]] <- dat.temp$r
      n[i, 1:na[i]] <- dat.temp$n
    }
    dat.jags <- list(NT = n.trt, NS = n.study, r = r, n = n, t = t, na = na)
  }

  if(is.element("fe", type)){
    set.seed(seed)
    inits.fe <- NULL
    for(i in 1:n.chains){
      inits.fe[[i]] <- list(mu = rep(rnorm(1), n.study),
        .RNG.name = "base::Wichmann-Hill", .RNG.seed = sample(123456789, 1))
    }
    params.fe <- c("lor", "totresdev")

    # FE NMA with evidence consistency
    set.seed(seed)
    jags.nma.fe.c <- jags.model(file = textConnection(nma.fe.c()),
      data = dat.jags, inits = inits.fe, n.chains = n.chains, n.adapt = n.adapt)
    update(jags.nma.fe.c, n.iter = n.burnin)
    coda.nma.fe.c <- coda.samples(model = jags.nma.fe.c,
      variable.names = params.fe, n.iter = n.iter, thin = n.thin)
    smry.nma.fe.c <- summary(coda.nma.fe.c)
    out.nma.fe.c <- smry.nma.fe.c$quantiles[grep("lor",
      rownames(smry.nma.fe.c$quantiles)), c("2.5%", "50%", "97.5%")]
    pD.nma.fe.c <- dic.samples(jags.nma.fe.c, n.iter = n.iter, thin = n.thin)

    # FE NMA with evidence inconsistency
    set.seed(seed)
    jags.nma.fe.ic <- jags.model(file = textConnection(nma.fe.ic()),
      data = dat.jags, inits = inits.fe, n.chains = n.chains, n.adapt = n.adapt)
    update(jags.nma.fe.ic, n.iter = n.burnin)
    coda.nma.fe.ic <- coda.samples(model = jags.nma.fe.ic,
      variable.names = params.fe, n.iter = n.iter, thin = n.thin)
    smry.nma.fe.ic <- summary(coda.nma.fe.ic)
    out.nma.fe.ic <- smry.nma.fe.ic$quantiles[grep("lor",
      rownames(smry.nma.fe.ic$quantiles)), c("2.5%", "50%", "97.5%")]
    pD.nma.fe.ic <- dic.samples(jags.nma.fe.ic, n.iter = n.iter, thin = n.thin)

    icdf.fe <- sum(pD.nma.fe.ic$penalty) - sum(pD.nma.fe.c$penalty)
    out$icdf.fe <- icdf.fe

    if(traceplot){
      # trace plots for FE NMA with evidence consistency
      for(i in 1:(n.trt - 1)){
        for(j in (i + 1):n.trt){
          filename <- paste0("traceplot_NMA_FE_C_",
            "lor[", i , ",", j, "]", ".png")
          if(!is.null(nma.name)){
            filename <- paste0(nma.name, "_", filename)
          }
          png(filename, res = 600, height = 8.5, width = 11, units = "in")
          par(mfrow = c(length(coda.nma.fe.c), 1))
          for(k in 1:length(coda.nma.fe.c)){
            temp <- as.vector(coda.nma.fe.c[[k]][,paste0("lor[", i, ",", j, "]")])
            plot(temp, type = "l", col = "red", xlab = "Iteration",
              ylab = paste0("lor[", i, ",", j, "]"), main = paste("Chain", k))
          }
          dev.off()
        }
      }

      # trace plots for FE NMA with evidence inconsistency
      for(i in 1:(n.trt - 1)){
        for(j in (i + 1):n.trt){
          filename <- paste0("traceplot_NMA_FE_IC_",
            "lor[", i , ",", j, "]", ".png")
          if(!is.null(nma.name)){
            filename <- paste0(nma.name, "_", filename)
          }
          png(filename, res = 600, height = 8.5, width = 11, units = "in")
          par(mfrow = c(length(coda.nma.fe.ic), 1))
          for(k in 1:length(coda.nma.fe.ic)){
            temp <- as.vector(coda.nma.fe.ic[[k]][,paste0("lor[", i, ",", j, "]")])
            plot(temp, type = "l", col = "red", xlab = "Iteration",
              ylab = paste0("lor[", i, ",", j, "]"), main = paste("Chain", k))
          }
          dev.off()
        }
      }
    }
  }

  if(is.element("re", type)){
    set.seed(seed)
    inits.re <- NULL
    for(i in 1:n.chains){
      inits.re[[i]] <- list(mu = rep(rnorm(1), n.study), tau = runif(1, 0, 2),
            .RNG.name = "base::Wichmann-Hill", .RNG.seed = sample(123456789, 1))
    }
    params.re <- c("lor", "tau", "totresdev")

    # RE NMA with evidence consistency
    set.seed(seed)
    jags.nma.re.c <- jags.model(file = textConnection(nma.re.c()),
      data = dat.jags, inits = inits.re, n.chains = n.chains, n.adapt = n.adapt)
    update(jags.nma.re.c, n.iter = n.burnin)
    coda.nma.re.c <- coda.samples(model = jags.nma.re.c,
      variable.names = params.re, n.iter = n.iter, thin = n.thin)
    smry.nma.re.c <- summary(coda.nma.re.c)
    out.nma.re.c <- smry.nma.re.c$quantiles[grep("lor",
      rownames(smry.nma.re.c$quantiles)), c("2.5%", "50%", "97.5%")]
    pD.nma.re.c <- dic.samples(jags.nma.re.c, n.iter = n.iter, thin = n.thin)

    # RE NMA with evidence inconsistency
    set.seed(seed)
    jags.nma.re.ic <- jags.model(file = textConnection(nma.re.ic()),
      data = dat.jags, inits = inits.re, n.chains = n.chains, n.adapt = n.adapt)
    update(jags.nma.re.ic, n.iter = n.burnin)
    coda.nma.re.ic <- coda.samples(model = jags.nma.re.ic,
      variable.names = params.re, n.iter = n.iter, thin = n.thin)
    smry.nma.re.ic <- summary(coda.nma.re.ic)
    out.nma.re.ic <- smry.nma.re.ic$quantiles[grep("lor",
      rownames(smry.nma.re.ic$quantiles)), c("2.5%", "50%", "97.5%")]
    pD.nma.re.ic <- dic.samples(jags.nma.re.ic, n.iter = n.iter, thin = n.thin)

    icdf.re <- sum(pD.nma.re.ic$penalty) - sum(pD.nma.re.c$penalty)
    out$icdf.re <- icdf.re

    if(traceplot){
      # trace plots for RE NMA with evidence consistency
      for(i in 1:(n.trt - 1)){
        for(j in (i + 1):n.trt){
          filename <- paste0("traceplot_NMA_RE_C_",
            "lor[", i , ",", j, "]", ".png")
          if(!is.null(nma.name)){
            filename <- paste0(nma.name, "_", filename)
          }
          png(filename, res = 600, height = 8.5, width = 11, units = "in")
          par(mfrow = c(length(coda.nma.re.c), 1))
          for(k in 1:length(coda.nma.re.c)){
            temp <- as.vector(coda.nma.re.c[[k]][,paste0("lor[", i, ",", j, "]")])
            plot(temp, type = "l", col = "red", xlab = "Iteration",
              ylab = paste0("lor[", i, ",", j, "]"), main = paste("Chain", k))
          }
          dev.off()
        }
      }

      # trace plots for RE NMA with evidence inconsistency
      for(i in 1:(n.trt - 1)){
        for(j in (i + 1):n.trt){
          filename <- paste0("traceplot_NMA_RE_IC_",
             "lor[", i , ",", j, "]", ".png")
          if(!is.null(nma.name)){
            filename <- paste0(nma.name, "_", filename)
          }
          png(filename, res = 600, height = 8.5, width = 11, units = "in")
          par(mfrow = c(length(coda.nma.re.ic), 1))
          for(k in 1:length(coda.nma.re.ic)){
            temp <- as.vector(coda.nma.re.ic[[k]][,paste0("lor[", i, ",", j, "]")])
            plot(temp, type = "l", col = "red", xlab = "Iteration",
              ylab = paste0("lor[", i, ",", j, "]"), main = paste("Chain", k))
          }
          dev.off()
        }
      }
    }
  }

  return(out)
}
