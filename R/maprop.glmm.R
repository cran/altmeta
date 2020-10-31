maprop.glmm <- function(e, n, data, link = "logit",
  alpha = 0.05, pop.avg = TRUE, int.approx = 10000, b.iter = 1000,
  seed = 1234, ...){
  if(missing(e)) stop("need to specify event counts.")
  if(missing(n)) stop("need to specify sample sizes.")
  if(!missing(data)){
    e <- eval(substitute(e), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if(length(e) != length(n)) stop("lengths of e and n differ.")
  if(!is.element(link, c("log", "logit", "probit", "cauchit", "cloglog"))){
    stop("cannot recognize the link function.")
  }
  if(alpha < 0 | alpha > 1) stop("alpha should be between 0 and 1.")
  N <- length(e)
  data <- data.frame(sid = 1:N, e = e, n = n)
  rslt <- glmer(cbind(e, n - e) ~ 1 + (1 | sid),
    data = data, family = binomial(link = link), ...)
  mu.est <- summary(rslt)$coefficients[1]
  mu.se <- summary(rslt)$coefficients[2]
  mu.ci <- c(mu.est - qnorm(1 - alpha/2) * mu.se,
    mu.est + qnorm(1 - alpha/2) * mu.se)
  tau.est <- sqrt(as.numeric(summary(rslt)$varcor))
  AICtab <- summary(rslt)$AICtab
  if(link == "log"){
    back.trans <- function(x) exp(x)
  }
  if(link == "logit"){
    back.trans <- function(x) plogis(x)
  }
  if(link == "probit"){
    back.trans <- function(x) pnorm(x)
  }
  if(link == "cauchit"){
    back.trans <- function(x) pcauchy(x)
  }
  if(link == "cloglog"){
    back.trans <- function(x) 1 - exp(-exp(x))
  }
  back.trans <- Vectorize(back.trans)
  prop.c.est <- back.trans(mu.est)
  prop.c.ci <- back.trans(mu.ci)
  out <- list(prop.c.est = prop.c.est, prop.c.ci = prop.c.ci, AICtab = AICtab)
  if(pop.avg){
    if(link == "probit"){
      prop.m.est <- pnorm(mu.est/sqrt(1 + tau.est^2))
    }else{
      set.seed(seed)
      stdnorms <- rnorm(int.approx)
      prop.m.est <- mean(back.trans(mu.est + tau.est * stdnorms))
    }
    mu.est.b <- prop.m.est.b <- rep(NA, b.iter)
    set.seed(seed)
    b.idx <- 0
    b.w <- b.e <- 0
    while(b.idx < b.iter){
      idx.temp <- sample(N, replace = TRUE)
      data.temp <- data[idx.temp,]
      data.temp$sid <- 1:N
      suppressMessages(out.W.E <- tryCatch.W.E(
        rslt.temp <- glmer(cbind(e, n - e) ~ 1 + (1 | sid),
          data = data.temp, family = binomial(link = link), ...)))
      if(is.null(out.W.E$warning) & !inherits(out.W.E$value, "error")){
        b.idx <- b.idx + 1
        mu.est.b[b.idx] <- summary(rslt.temp)$coefficients[1]
        tau.est.temp <- sqrt(as.numeric(summary(rslt.temp)$varcor))
        if(link == "probit"){
          prop.m.est.b[b.idx] <-
            pnorm(mu.est.b[b.idx]/sqrt(1 + tau.est.temp^2))
        }else{
          prop.m.est.b[b.idx] <- mean(back.trans(mu.est.b[b.idx] +
            tau.est.temp * stdnorms))
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
    prop.c.ci.b <- back.trans(mu.ci.b)
    prop.m.ci.b <- quantile(prop.m.est.b, probs = c(alpha/2, 1 - alpha/2))
    names(prop.m.ci.b) <- NULL
    out <- c(out, list(prop.c.ci.b = prop.c.ci.b,
      prop.m.est = prop.m.est, prop.m.ci.b = prop.m.ci.b,
      b.w.e = c("bootstrap warnings" = b.w, "bootstrap errors" = b.e)))
  }
  return(out)
}