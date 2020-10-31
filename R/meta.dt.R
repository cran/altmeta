meta.dt <- function(tp, fp, fn, tn, data, method = "biv.glmm", alpha = 0.05, ...){
  if(missing(tp)) stop("need to specify counts of true positives.")
  if(missing(fp)) stop("need to specify counts of false positives.")
  if(missing(fn)) stop("need to specify counts of false negatives.")
  if(missing(tn)) stop("need to specify counts of true negatives.")
  if(!missing(data)){
    tp <- eval(substitute(tp), data, parent.frame())
    fp <- eval(substitute(fp), data, parent.frame())
    fn <- eval(substitute(fn), data, parent.frame())
    tn <- eval(substitute(tn), data, parent.frame())
  }
  if((length(tp) != length(fp)) | (length(tp) != length(fn)) | (length(tp) != length(tn))){
    stop("the input data have different dimensions.")
  }
  if(!is.element(method, c("s.roc", "biv.lmm", "biv.glmm"))){
    stop("do not recognize the specified method.")
  }

  idx.allzero <- which(tp == 0 & fp == 0 & fn == 0 & tn == 0)
  if(length(idx.allzero) > 0){
    tp <- tp[-idx.allzero]
    fp <- fp[-idx.allzero]
    fn <- fn[-idx.allzero]
    tn <- tn[-idx.allzero]
    message("studies with zero counts in all four data cells are removed.")
  }
  if(length(tp) < 2) stop("less than two studies in the meta-analysis.")
  dat.ori <- data.frame(tp = tp, fp = fp, fn = fn, tn = tn)
  n.stu <- length(tp)
  sid <- rep(1:n.stu, each = 2)
  sens.id <- rep(c(1, 0), n.stu)

  if(method == "s.roc"){
    for(i in 1:n.stu){
      if(tp[i] == 0 | fp[i] == 0 | fn[i] == 0 | tn[i] == 0){
        tp[i] <- tp[i] + 0.5
        fp[i] <- fp[i] + 0.5
        fn[i] <- fn[i] + 0.5
        tn[i] <- tn[i] + 0.5
      }
    }
    sens <- tp/(tp + fn)
    spec <- tn/(fp + tn)
    y1 <- log(sens/(1 - sens))
    y2 <- log(spec/(1 - spec))
    v1 <- 1/tp + 1/fn
    v2 <- 1/fp + 1/tn
    D <- y1 + y2
    S <- y1 - y2
    V <- v1 + v2

    lm.unwtd <- lm(D ~ S, ...)
    smry.unwtd <- summary(lm.unwtd)
    inter.unwtd <- smry.unwtd$coefficients[1,1]
    slope.unwtd <- smry.unwtd$coefficients[2,1]
    vcov.unwtd <- vcov(lm.unwtd)
    logDOR.meanS.unwtd <- inter.unwtd + slope.unwtd*mean(S)
    temp.coef <- c(1, mean(S))
    logDOR.meanS.unwtd.var <- as.numeric(t(temp.coef) %*% vcov.unwtd %*% temp.coef)
    logDOR.meanS.unwtd.ci <- c(logDOR.meanS.unwtd - qnorm(1 - alpha/2)*sqrt(logDOR.meanS.unwtd.var),
      logDOR.meanS.unwtd + qnorm(1 - alpha/2)*sqrt(logDOR.meanS.unwtd.var))
    Q.unwtd <- 1/(1 + exp(-inter.unwtd/2))
    inter.unwtd.ci <- c(inter.unwtd - qnorm(1 - alpha/2)*sqrt(vcov.unwtd[1,1]),
      inter.unwtd + qnorm(1 - alpha/2)*sqrt(vcov.unwtd[1,1]))
    Q.unwtd.ci <- 1/(1 + exp(-inter.unwtd.ci/2))
    roc.curve.unwtd <- function(xval){
      spec <- 1 - xval
      sens <- 1/(1 + exp(-inter.unwtd/(1 - slope.unwtd))*(spec/(1 - spec))^((1 + slope.unwtd)/(1 - slope.unwtd)))
      return(sens)
    }
    AUC.unwtd <- integrate(roc.curve.unwtd, lower = 0, upper = 1)$value

    lm.wtd <- lm(D ~ S, weights = 1/V, ...)
    smry.wtd <- summary(lm.wtd)
    inter.wtd <- smry.wtd$coefficients[1,1]
    slope.wtd <- smry.wtd$coefficients[2,1]
    vcov.wtd <- vcov(lm.wtd)
    logDOR.meanS.wtd <- inter.wtd + slope.wtd*mean(S)
    logDOR.meanS.wtd.var <- as.numeric(t(temp.coef) %*% vcov.wtd %*% temp.coef)
    logDOR.meanS.wtd.ci <- c(logDOR.meanS.wtd - qnorm(1 - alpha/2)*sqrt(logDOR.meanS.wtd.var),
      logDOR.meanS.wtd + qnorm(1 - alpha/2)*sqrt(logDOR.meanS.wtd.var))
    Q.wtd <- 1/(1 + exp(-inter.wtd/2))
    inter.wtd.ci <- c(inter.wtd - qnorm(1 - alpha/2)*sqrt(vcov.wtd[1,1]),
      inter.wtd + qnorm(1 - alpha/2)*sqrt(vcov.wtd[1,1]))
    Q.wtd.ci <- 1/(1 + exp(-inter.wtd.ci/2))
    roc.curve.wtd <- function(xval){
      spec <- 1 - xval
      sens <- 1/(1 + exp(-inter.wtd/(1 - slope.wtd))*(spec/(1 - spec))^((1 + slope.wtd)/(1 - slope.wtd)))
      return(sens)
    }
    AUC.wtd <- integrate(roc.curve.wtd, lower = 0, upper = 1)$value

    out <- list(method = method, alpha = alpha,
      inter.unwtd = inter.unwtd, slope.unwtd = slope.unwtd, vcov.unwtd = vcov.unwtd,
      DOR.meanS.unwtd = exp(logDOR.meanS.unwtd), DOR.meanS.unwtd.ci = exp(logDOR.meanS.unwtd.ci),
      Q.unwtd = Q.unwtd, Q.unwtd.ci = Q.unwtd.ci, AUC.unwtd = AUC.unwtd,
      inter.wtd = inter.wtd, slope.wtd = slope.wtd, vcov.wtd = vcov.wtd,
      DOR.meanS.wtd = exp(logDOR.meanS.wtd), DOR.meanS.wtd.ci = exp(logDOR.meanS.wtd.ci),
      Q.wtd = Q.wtd, Q.wtd.ci = Q.wtd.ci, AUC.wtd = AUC.wtd, data = dat.ori)
  }

  if(is.element(method, c("biv.lmm", "biv.glmm"))){
    if(method == "biv.lmm"){
      for(i in 1:n.stu){
        if(tp[i] == 0 | fp[i] == 0 | fn[i] == 0 | tn[i] == 0){
          tp[i] <- tp[i] + 0.5
          fp[i] <- fp[i] + 0.5
          fn[i] <- fn[i] + 0.5
          tn[i] <- tn[i] + 0.5
        }
      }
      sens <- tp/(tp + fn)
      spec <- tn/(fp + tn)
      y1 <- log(sens/(1 - sens))
      y2 <- log(spec/(1 - spec))
      y <- c(rbind(y1, y2))
      v1 <- 1/tp + 1/fn
      v2 <- 1/fp + 1/tn
      v <- c(rbind(v1, v2))
      dat <- data.frame(sid = sid, sens.id = factor(sens.id), y = y, v = v)
      rslt <- rma.mv(yi = y, V = v, mods = ~ sens.id, random = ~ sens.id | sid, struct = "HCS", data = dat, ...)
      mu.sens <- rslt$beta[1,1] + rslt$beta[2,1]
      mu.spec <- rslt$beta[1,1]
      names(mu.sens) <- names(mu.spec) <- NULL
      temp.vcov <- rslt$vb
      rownames(temp.vcov) <- colnames(temp.vcov) <- NULL
      C.mat <- matrix(c(1, 1, 1, 0), 2, 2)
      mu.vcov <- C.mat %*% temp.vcov %*% t(C.mat)
      sig.sens <- sqrt(rslt$tau2[2])
      sig.spec <- sqrt(rslt$tau2[1])
      rho <- rslt$rho
    }

    if(method == "biv.glmm"){
      e <- c(rbind(tp, tn))
      tot.p <- tp + fn
      tot.n <- fp + tn
      tot <- c(rbind(tot.p, tot.n))
      dat <- data.frame(sid = sid, sens.id = sens.id, spec.id = 1 - sens.id, e = e, tot = tot)
      rslt <- glmer(cbind(e, tot - e) ~ sens.id + spec.id - 1 + (sens.id + spec.id - 1 | sid),
        data = dat, family = binomial(link = "logit"), ...)
      smry <- summary(rslt)
      mu.sens <- smry$coefficients[1,1]
      mu.spec <- smry$coefficients[2,1]
      mu.vcov <- matrix(as.numeric(smry$vcov), 2, 2)
      sig.sens <- attr(smry$varcor[[1]], "stddev")[1]
      sig.spec <- attr(smry$varcor[[1]], "stddev")[2]
      rho <- attr(smry$varcor[[1]], "correlation")[1,2]
      names(sig.sens) <- names(sig.spec) <- NULL
    }

    sens.overall <- 1/(1 + exp(-mu.sens))
    mu.sens.ci <- c(mu.sens - qnorm(1 - alpha/2)*sqrt(mu.vcov[1,1]),
      mu.sens + qnorm(1 - alpha/2)*sqrt(mu.vcov[1,1]))
    sens.overall.ci <- 1/(1 + exp(-mu.sens.ci))
    spec.overall <- 1/(1 + exp(-mu.spec))
    mu.spec.ci <- c(mu.spec - qnorm(1 - alpha/2)*sqrt(mu.vcov[2,2]),
      mu.spec + qnorm(1 - alpha/2)*sqrt(mu.vcov[2,2]))
    spec.overall.ci <- 1/(1 + exp(-mu.spec.ci))
    logDOR.overall <- mu.sens + mu.spec
    logDOR.overall.stderr <- sqrt(sum(mu.vcov))
    logDOR.overall.ci <- c(logDOR.overall - qnorm(1 - alpha/2)*logDOR.overall.stderr,
      logDOR.overall + qnorm(1 - alpha/2)*logDOR.overall.stderr)
    DOR.overall <- exp(logDOR.overall)
    DOR.overall.ci <- exp(logDOR.overall.ci)
    roc.curve <- function(xval){
      spec <- 1 - xval
      logit.spec <- log(spec/(1 - spec))
      logit.sens <- mu.sens + rho*sig.sens/sig.spec*(logit.spec - mu.spec)
      sens <- 1/(1 + exp(-logit.sens))
      return(sens)
    }
    AUC <- integrate(roc.curve, lower = 0, upper = 1)$value

    out <- list(method = method, alpha = alpha,
      sens.overall = sens.overall, sens.overall.ci = sens.overall.ci,
      spec.overall = spec.overall, spec.overall.ci = spec.overall.ci,
      DOR.overall = DOR.overall, DOR.overall.ci = DOR.overall.ci, AUC = AUC,
      mu.sens = mu.sens, mu.spec = mu.spec, mu.vcov = mu.vcov,
      sig.sens = sig.sens, sig.spec = sig.spec, rho = rho, data = dat.ori)
  }

  class(out) <- "meta.dt"
  return(out)
}
