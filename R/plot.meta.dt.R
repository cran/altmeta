plot.meta.dt <- function(x, add = FALSE, xlab, ylab, alpha,
  studies = TRUE, cex.studies, col.studies, pch.studies,
  roc, col.roc, lty.roc, lwd.roc, weight = FALSE,
  eqline, col.eqline, lty.eqline, lwd.eqline,
  overall = TRUE, cex.overall, col.overall, pch.overall,
  confid = TRUE, col.confid, lty.confid, lwd.confid,
  predict = FALSE, col.predict, lty.predict, lwd.predict, ...){
  if(!inherits(x, "meta.dt")){
    stop("The input must be an object of class \"meta.dt\".")
  }
  if(x$method == "s.roc"){
    overall <- confid <- predict <- FALSE
    if(missing(eqline)) eqline <- TRUE
    if(missing(roc)) roc <- TRUE
  }else{
    if(missing(eqline)) eqline <- FALSE
    if(missing(roc)) roc <- FALSE
  }

  if(!add){
    if(missing(xlab)) xlab <- "1 - Specificity"
    if(missing(ylab)) ylab <- "Sensitivity"
    plot.default(0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
      xlab = xlab, ylab = ylab, ...)
  }

  if(studies){
    dat <- x$data
    tp <- dat$tp
    fp <- dat$fp
    fn <- dat$fn
    tn <- dat$tn
    xval <- 1 - tn/(fp + tn)
    yval <- tp/(tp + fn)
    if(missing(cex.studies)) cex.studies <- 1
    if(missing(col.studies)) col.studies <- "black"
    if(missing(pch.studies)) pch.studies <- 1
    points(xval, yval, cex = cex.studies, col = col.studies, pch = pch.studies)
  }

  if(roc){
    if(missing(col.roc)) col.roc <- "black"
    if(missing(lty.roc)) lty.roc <- 1
    if(missing(lwd.roc)) lwd.roc <- 1
    if(x$method == "s.roc"){
      if(!weight){
        inter <- x$inter.unwtd
        slope <- x$slope.unwtd
      }else{
        inter <- x$inter.wtd
        slope <- x$slope.wtd
      }
      roc.curve <- function(xval){
        spec <- 1 - xval
        sens <- 1/(1 + exp(-inter/(1 - slope))*(spec/(1 - spec))^((1 + slope)/(1 - slope)))
        return(sens)
      }
    }
    if(is.element(x$method, c("biv.lmm", "biv.glmm"))){
      mu.sens <- x$mu.sens
      mu.spec <- x$mu.spec
      sig.sens <- x$sig.sens
      sig.spec <- x$sig.spec
      rho <- x$rho
      roc.curve <- function(xval){
        spec <- 1 - xval
        logit.spec <- log(spec/(1 - spec))
        logit.sens <- mu.sens + rho*sig.sens/sig.spec*(logit.spec - mu.spec)
        sens <- 1/(1 + exp(-logit.sens))
        return(sens)
      }
    }
    curve(roc.curve, from = 0, to = 1, add = TRUE,
      col = col.roc, lty = lty.roc, lwd = lwd.roc)
  }

  if(eqline){
    if(missing(col.eqline)) col.eqline <- "black"
    if(missing(lty.eqline)) lty.eqline <- 4
    if(missing(lwd.eqline)) lwd.eqline <- 1
    eqline.fcn <- function(x) 1 - x
    curve(eqline.fcn, from = 0, to = 1, add = TRUE,
      col = col.eqline, lty = lty.eqline, lwd = lwd.eqline)
  }

  if(overall){
    if(missing(cex.overall)) cex.overall <- 1
    if(missing(col.overall)) col.overall <- "black"
    if(missing(pch.overall)) pch.overall <- 15
    points(1 - x$spec.overall, x$sens.overall,
      cex = cex.overall, col = col.overall, pch = pch.overall)
  }

  if(confid){
    if(missing(alpha)) alpha <- x$alpha
    mu.sens <- x$mu.sens
    mu.spec <- x$mu.spec
    mu.vcov <- x$mu.vcov
    mu.vcov.inv <- solve(mu.vcov)
    confid.contour <- function(xval, yval){
      sens <- yval
      spec <- 1 - xval
      logit.sens <- log(sens/(1 - sens))
      logit.spec <- log(spec/(1 - spec))
      out <- mu.vcov.inv[1,1]*(logit.sens - mu.sens)^2 + mu.vcov.inv[2,2]*(logit.spec - mu.spec)^2 +
        2*mu.vcov.inv[1,2]*(logit.sens - mu.sens)*(logit.spec - mu.spec) - qchisq(1 - alpha, df = 2)
      return(out)
    }
    xval <- yval <- seq(0, 1, length = 1000)
    zval <- outer(xval, yval, confid.contour)
    if(missing(col.confid)) col.confid <- "black"
    if(missing(lty.confid)) lty.confid <- 2
    if(missing(lwd.confid)) lwd.confid <- 1
    contour(x = xval, y = yval, z = zval, levels = 0, drawlabels = FALSE, axes = FALSE,
     col = col.confid, lty = lty.confid, lwd = lwd.confid, add = TRUE)
  }

  if(predict){
    if(missing(alpha)) alpha <- x$alpha
    mu.sens <- x$mu.sens
    mu.spec <- x$mu.spec
    mu.vcov <- x$mu.vcov
    sig.sens <- x$sig.sens
    sig.spec <- x$sig.spec
    rho <- x$rho
    re.mat <- matrix(c(sig.sens^2, rho*sig.sens*sig.spec, rho*sig.sens*sig.spec, sig.spec^2), 2, 2)
    tot.vcov <- mu.vcov + re.mat
    tot.vcov.inv <- solve(tot.vcov)
    predict.contour <- function(xval, yval){
      sens <- yval
      spec <- 1 - xval
      logit.sens <- log(sens/(1 - sens))
      logit.spec <- log(spec/(1 - spec))
      out <- tot.vcov.inv[1,1]*(logit.sens - mu.sens)^2 + tot.vcov.inv[2,2]*(logit.spec - mu.spec)^2 +
        2*tot.vcov.inv[1,2]*(logit.sens - mu.sens)*(logit.spec - mu.spec) - qchisq(1 - alpha, df = 2)
      return(out)
    }
    xval <- yval <- seq(0, 1, length = 1000)
    zval <- outer(xval, yval, predict.contour)
    if(missing(col.predict)) col.predict <- "black"
    if(missing(lty.predict)) lty.predict <- 3
    if(missing(lwd.predict)) lwd.predict <- 1
    contour(x = xval, y = yval, z = zval, levels = 0, drawlabels = FALSE, axes = FALSE,
      col = col.predict, lty = lty.predict, lwd = lwd.predict, add = TRUE)
  }
}