meta.biv <- function(sid, tid, e, n, data, link = "logit",
  alpha = 0.05, b.iter = 1000, seed = 1234, ...){
  if(missing(sid)) stop("need to specify study IDs.")
  if(missing(tid)) stop("need to specify treatment IDs (0/1).")
  if(missing(e)) stop("need to specify event counts.")
  if(missing(n)) stop("need to specify sample sizes.")
  if(!missing(data)){
    sid <- eval(substitute(sid), data, parent.frame())
    tid <- eval(substitute(tid), data, parent.frame())
    e <- eval(substitute(e), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if((length(sid) != length(n)) | (length(tid) != length(n)) | (length(e) != length(n))){
    stop("the input data have different dimensions.")
  }
  if(!is.element(link, c("logit", "probit"))){
    stop("specify either the logit or probit link function.")
  }
  if(alpha < 0 | alpha > 1) stop("alpha should be between 0 and 1.")

  ord <- order(sid)
  sid <- sid[ord]
  tid <- tid[ord]
  e <- e[ord]
  n <- n[ord]
  data <- data.frame(sid = sid, tid = tid, cid = 1 - tid, e = e, n = n)
  rslt <- glmer(cbind(e, n - e) ~ factor(tid) + (cid + tid - 1 | sid),
    data = data, family = binomial(link = link), ...)
  smry <- summary(rslt)
  mu0 <- smry$coefficients[1,1]
  mu1 <- smry$coefficients[2,1] + mu0
  sd0 <- attr(smry$varcor[[1]], "stddev")[1]
  sd1 <- attr(smry$varcor[[1]], "stddev")[2]
  rho <- attr(smry$varcor[[1]], "correlation")[1,2]
  names(sd0) <- names(sd1) <- NULL

  if(link == "logit"){
    logOR.c <- summary(rslt)$coefficients[2,1]
    logOR.c.se <- summary(rslt)$coefficients[2,2]
    OR.c <- exp(logOR.c)
    OR.c.ci <- exp(c(logOR.c - qnorm(1 - alpha/2)*logOR.c.se,
      logOR.c + qnorm(1 - alpha/2)*logOR.c.se))
    C <- 16*sqrt(3)/(15*3.1415926)
    p0.m <- 1/(1 + exp(-mu0/sqrt(1 + C^2*sd0^2)))
    p1.m <- 1/(1 + exp(-mu1/sqrt(1 + C^2*sd1^2)))

    out <- list(OR.c = OR.c, OR.c.ci = OR.c.ci)
  }

  if(link == "probit"){
    p0.m <- pnorm(mu0/sqrt(1 + sd0^2))
    p1.m <- pnorm(mu1/sqrt(1 + sd1^2))
    out <- NULL
  }

  OR.m <- p1.m/(1 - p1.m)/p0.m*(1 - p0.m)
  RR.m <- p1.m/p0.m
  RD.m <- p1.m - p0.m

  p0.m.b <- p1.m.b <- OR.m.b <- RR.m.b <- RD.m.b <- rho.b <- rep(NA, b.iter)
  set.seed(seed)
  b.idx <- 0
  b.w <- b.e <- 0
  while(b.idx < b.iter){
    idx.temp <- sample(length(unique(sid)), replace = TRUE)
    idx.temp <- c(rbind(idx.temp*2 - 1, idx.temp*2))
    data.temp <- data[idx.temp,]
    data.temp$sid <- rep(1:length(unique(sid)), each = 2)
    suppressMessages(out.W.E <- tryCatch.W.E(
      rslt.temp <- glmer(cbind(e, n - e) ~ factor(tid) + (cid + tid - 1 | sid),
        data = data.temp, family = binomial(link = link), ...)))
    suppressMessages(out.W.E2 <- tryCatch.W.E(smry.temp <- summary(rslt.temp)))
    if(is.null(out.W.E$warning) & !inherits(out.W.E$value, "error") &
      is.null(out.W.E2$warning) & !inherits(out.W.E2$value, "error")){
      b.idx <- b.idx + 1
      mu0.temp <- smry.temp$coefficients[1,1]
      mu1.temp <- smry.temp$coefficients[2,1] + mu0.temp
      sd0.temp <- attr(smry.temp$varcor[[1]], "stddev")[1]
      sd1.temp <- attr(smry.temp$varcor[[1]], "stddev")[2]
      rho.b[b.idx] <- attr(smry.temp$varcor[[1]], "correlation")[1,2]
      if(link == "logit"){
        C <- 16*sqrt(3)/(15*3.1415926)
        p0.m.temp <- 1/(1 + exp(-mu0.temp/sqrt(1 + C^2*sd0.temp^2)))
        p1.m.temp <- 1/(1 + exp(-mu1.temp/sqrt(1 + C^2*sd1.temp^2)))
      }
      if(link == "probit"){
        p0.m.temp <- pnorm(mu0.temp/sqrt(1 + sd0.temp^2))
        p1.m.temp <- pnorm(mu1.temp/sqrt(1 + sd1.temp^2))
      }
      p0.m.b[b.idx] <- p0.m.temp
      p1.m.b[b.idx] <- p1.m.temp
      OR.m.b[b.idx] <- p1.m.temp/(1 - p1.m.temp)/p0.m.temp*(1 - p0.m.temp)
      RR.m.b[b.idx] <- p1.m.temp/p0.m.temp
      RD.m.b[b.idx] <- p1.m.temp - p0.m.temp
    }
    if(!is.null(out.W.E$warning) | !is.null(out.W.E2$warning)){
      b.w <- b.w + 1
    }
    if(inherits(out.W.E$value, "error") | inherits(out.W.E2$value, "error")){
      b.e <- b.e + 1
    }
  }
  p0.m.ci <- quantile(p0.m.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  p1.m.ci <- quantile(p1.m.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  OR.m.ci <- quantile(OR.m.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  RR.m.ci <- quantile(RR.m.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  RD.m.ci <- quantile(RD.m.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  rho.ci <- quantile(rho.b, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  names(p0.m.ci) <- names(p1.m.ci) <- names(OR.m.ci) <- names(RR.m.ci) <-
    names(RD.m.ci) <- names(rho.ci) <- NULL

  out <- c(out, list(p0.m = p0.m, p0.m.ci = p0.m.ci, p1.m = p1.m, p1.m.ci = p1.m.ci,
    OR.m = OR.m, OR.m.ci = OR.m.ci, RR.m = RR.m, RR.m.ci = RR.m.ci,
    RD.m = RD.m, RD.m.ci = RD.m.ci, rho = rho, rho.ci = rho.ci,
    b.w.e = c("bootstrap warnings" = b.w, "bootstrap errors" = b.e)))
  return(out)
}
