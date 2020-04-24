maprop.twostep <- function(e, n, data, link = "logit", method = "ML",
  alpha = 0.05, pop.avg = TRUE, int.approx = 10000, b.iter = 1000,
  seed = 1234){
  if(missing(e)) stop("need to specify event counts.")
  if(missing(n)) stop("need to specify sample sizes.")
  if(!missing(data)){
    e <- eval(substitute(e), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if(length(e) != length(n)) stop("lengths of e and n differ.")
  if(!is.element(link, c("log", "logit", "arcsine", "double.arcsine"))){
    stop("cannot recognize the data transformation (link).")
  }
  if(alpha < 0 | alpha > 1) stop("alpha should be between 0 and 1.")
  N <- length(e)
  if(any(e == 0) | any(e == n)){
    e <- e + 0.5
    n <- n + 1
  }
  p <- e/n
  if(link == "log"){
    y <- log(p)
    v <- 1/e - 1/n
    back.trans <- function(x) exp(x)
    back.trans <- Vectorize(back.trans)
  }
  if(link == "logit"){
    y <- log(p/(1 - p))
    v <- 1/e + 1/(n - e)
    back.trans <- function(x) exp(x)/(1 + exp(x))
    back.trans <- Vectorize(back.trans)
  }
  if(link == "arcsine"){
    y <- asin(sqrt(p))
    v <- 1/(4 * n)
    back.trans <- function(x) (sin(x))^2
    back.trans <- Vectorize(back.trans)
  }
  if(link == "double.arcsine"){
    y <- asin(sqrt(e/(n + 1))) + asin(sqrt((e + 1)/(n + 1)))
    v <- 1/(n + 0.5)
    back.trans.da <- function(x, pooled.n){
      if(x > asin(sqrt(1/(pooled.n + 1))) &
        x < (asin(sqrt(pooled.n/(pooled.n + 1))) + asin(1))){
        output <- 0.5 * (1 - sign(cos(x)) * sqrt(
          1 - (sin(x) + (sin(x) - 1/sin(x))/pooled.n)^2))
      }else{
        output <- (sin(x/2))^2
      }
      return(output)
    }
    back.trans.da <- Vectorize(back.trans.da)
    pooled.n.harmonic <- 1/mean(1/n)
    pooled.n.geometric <- exp(mean(log(n)))
    pooled.n.arithmetic <- mean(n)
  }
  rslt <- rma(yi = y, vi = v, method = method)
  mu.est <- as.numeric(rslt$beta)
  mu.se <- rslt$se
  mu.ci <- c(mu.est - qnorm(1 - alpha/2) * mu.se,
    mu.est + qnorm(1 - alpha/2) * mu.se)
  tau.est <- sqrt(rslt$tau2)
  if(link != "double.arcsine"){
    prop.c.est <- back.trans(mu.est)
    prop.c.ci <- back.trans(mu.ci)
  }
  if(link == "double.arcsine"){
    pooled.n.invvar <- 1/mu.se^2
    prop.c.est <- c(
      "harmonic" = back.trans.da(mu.est, pooled.n.harmonic),
      "geometric" = back.trans.da(mu.est, pooled.n.geometric),
      "arithmetic" = back.trans.da(mu.est, pooled.n.arithmetic),
      "invvar" = back.trans.da(mu.est, pooled.n.invvar))
    prop.c.ci <- rbind(
      "harmonic" = back.trans.da(mu.ci, pooled.n.harmonic),
      "geometric" = back.trans.da(mu.ci, pooled.n.geometric),
      "arithmetic" = back.trans.da(mu.ci, pooled.n.arithmetic),
      "invvar" = back.trans.da(mu.ci, pooled.n.invvar))
  }
  out <- list(prop.c.est = prop.c.est, prop.c.ci = prop.c.ci)
  if(pop.avg){
    set.seed(seed)
    stdnorms <- rnorm(int.approx)
    if(link != "double.arcsine"){
      prop.m.est <- mean(back.trans(mu.est + tau.est * stdnorms))
      mu.est.b <- prop.m.est.b <- rep(NA, b.iter)
    }
    if(link == "double.arcsine"){
      prop.m.est <- c(
        "harmonic" = mean(back.trans.da(mu.est + tau.est * stdnorms,
          pooled.n.harmonic)),
        "geometric" = mean(back.trans.da(mu.est + tau.est * stdnorms,
          pooled.n.geometric)),
        "arithmetic" = mean(back.trans.da(mu.est + tau.est * stdnorms,
          pooled.n.arithmetic)),
        "invvar" = mean(back.trans.da(mu.est + tau.est * stdnorms,
          pooled.n.invvar)))
      mu.est.b <- rep(NA, b.iter)
      prop.m.est.b <- matrix(NA, b.iter, 4)
    }
    set.seed(seed)
    b.idx <- 0
    b.w <- b.e <- 0
    while(b.idx < b.iter){
      idx.temp <- sample(N, replace = TRUE)
      y.temp <- y[idx.temp]
      v.temp <- v[idx.temp]
      suppressMessages(out.W.E <- tryCatch.W.E(
        rslt.temp <- rma(yi = y.temp, vi = v.temp, method = method)))
      if(is.null(out.W.E$warning) & !inherits(out.W.E$value, "error")){
        b.idx <- b.idx + 1
        mu.est.b[b.idx] <- as.numeric(rslt.temp$beta)
        tau.est.temp <- sqrt(rslt.temp$tau2)
        if(link != "double.arcsine"){
          prop.m.est.b[b.idx] <- mean(back.trans(mu.est.b[b.idx] +
            tau.est.temp * stdnorms))
        }
        if(link == "double.arcsine"){
          prop.m.est.b[b.idx,1] <- mean(back.trans.da(mu.est.b[b.idx] +
            tau.est.temp * stdnorms, pooled.n.harmonic))
          prop.m.est.b[b.idx,2] <- mean(back.trans.da(mu.est.b[b.idx] +
            tau.est.temp * stdnorms, pooled.n.geometric))
          prop.m.est.b[b.idx,3] <- mean(back.trans.da(mu.est.b[b.idx] +
            tau.est.temp * stdnorms, pooled.n.arithmetic))
          prop.m.est.b[b.idx,4] <- mean(back.trans.da(mu.est.b[b.idx] +
            tau.est.temp * stdnorms, pooled.n.invvar))
        }
      }
      if(!is.null(out.W.E$warning)){
        b.w <- b.w + 1
      }
      if(inherits(out.W.E$value, "error")){
        b.e <- b.e + 1
      }
    }
    mu.ci.b <- quantile(mu.est.b, probs = c(alpha/2, 1 - alpha/2))
    names(mu.ci.b) <- NULL
    if(link != "double.arcsine"){
      prop.c.ci.b <- back.trans(mu.ci.b)
      prop.m.ci.b <- quantile(prop.m.est.b, probs = c(alpha/2, 1 - alpha/2))
      names(prop.m.ci.b) <- NULL
    }
    if(link == "double.arcsine"){
      prop.c.ci.b <- rbind(
        "harmonic" = back.trans.da(mu.ci.b, pooled.n.harmonic),
        "geometric" = back.trans.da(mu.ci.b, pooled.n.geometric),
        "arithmetic" = back.trans.da(mu.ci.b, pooled.n.arithmetic),
        "invvar" = back.trans.da(mu.ci.b, pooled.n.invvar))
      prop.m.ci.b <- rbind(
        "harmonic" = quantile(prop.m.est.b[,1],
          probs = c(alpha/2, 1 - alpha/2)),
        "geometric" = quantile(prop.m.est.b[,2],
          probs = c(alpha/2, 1 - alpha/2)),
        "arithmetic" = quantile(prop.m.est.b[,3],
          probs = c(alpha/2, 1 - alpha/2)),
        "invvar" = quantile(prop.m.est.b[,4],
          probs = c(alpha/2, 1 - alpha/2)))
      colnames(prop.m.ci.b) <- NULL
    }
    out <- c(out, list(prop.c.ci.b = prop.c.ci.b,
      prop.m.est = prop.m.est, prop.m.ci.b = prop.m.ci.b))
    if(link == "double.arcsine"){
      out <- c(out, list(pooled.n = 
        c("harmonic" = pooled.n.harmonic,
        "geometric" = pooled.n.geometric,
        "arithmetic" = pooled.n.arithmetic,
        "invvar" = pooled.n.invvar)))
    }
    out <- c(out, list(b.w.e =
      c("bootstrap warnings" = b.w, "bootstrap errors" = b.e)))
  }
  return(out)
}