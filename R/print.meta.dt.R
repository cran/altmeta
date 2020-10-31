print.meta.dt <- function(x, digits = 3, ...){
  if(!inherits(x, "meta.dt")){
    stop("The input must be an object of \"meta.dt\".")
  }

  cat(paste0("The meta-analysis contains ", dim(x$data)[1], " studies\n"))
  alpha <- x$alpha
  cat(paste0("Significance level: ", alpha, "\n"))
  if(x$method == "s.roc"){
    cat("The summary ROC approach is used\n")

    cat("Unweighted regression:\n")
    inter <- x$inter.unwtd
    inter.se <- sqrt(x$vcov.unwtd[1,1])
    inter.ci <- c(inter - qnorm(1 - alpha/2)*inter.se, inter + qnorm(1 - alpha/2)*inter.se)
    inter <- format(round(inter, digits), nsmall = digits)
    inter.se <- format(round(inter.se, digits), nsmall = digits)
    inter.ci <- format(round(inter.ci, digits), nsmall = digits)
    inter.ci <- gsub(" ", "", inter.ci)
    slope <- x$slope.unwtd
    slope.se <- sqrt(x$vcov.unwtd[2,2])
    slope.ci <- c(slope - qnorm(1 - alpha/2)*slope.se, slope + qnorm(1 - alpha/2)*slope.se)
    slope <- format(round(slope, digits), nsmall = digits)
    slope.se <- format(round(slope.se, digits), nsmall = digits)
    slope.ci <- format(round(slope.ci, digits), nsmall = digits)
    slope.ci <- gsub(" ", "", slope.ci)
    DOR.meanS <- x$DOR.meanS.unwtd
    DOR.meanS.ci <- x$DOR.meanS.unwtd.ci
    DOR.meanS <- format(round(DOR.meanS, digits), nsmall = digits)
    DOR.meanS.ci <- format(round(DOR.meanS.ci, digits), nsmall = digits)
    DOR.meanS.ci <- gsub(" ", "", DOR.meanS.ci)
    Q <- x$Q.unwtd
    Q.ci <- x$Q.unwtd.ci
    Q <- format(round(Q, digits), nsmall = digits)
    Q.ci <- format(round(Q.ci, digits), nsmall = digits)
    Q.ci <- gsub(" ", "", Q.ci)
    AUC <- x$AUC.unwtd
    AUC <- format(round(AUC, digits), nsmall = digits)
    cat(paste0("  intercept = ", inter, " (SE, ", inter.se, "; CI, ", inter.ci[1], " to ", inter.ci[2], ")\n"))
    cat(paste0("  slope = ", slope, " (SE, ", slope.se, "; CI, ", slope.ci[1], " to ", slope.ci[2], ")\n"))
    cat(paste0("  DOR = ", DOR.meanS, " (CI, ", DOR.meanS.ci[1], " to ", DOR.meanS.ci[2], ")\n"))
    cat(paste0("  Q point = ", Q, " (CI, ", Q.ci[1], " to ", Q.ci[2], ")\n"))
    cat(paste0("  AUC = ", AUC, "\n"))

    cat("Weighted regression:\n")
    inter <- x$inter.wtd
    inter.se <- sqrt(x$vcov.wtd[1,1])
    inter.ci <- c(inter - qnorm(1 - alpha/2)*inter.se, inter + qnorm(1 - alpha/2)*inter.se)
    inter <- format(round(inter, digits), nsmall = digits)
    inter.se <- format(round(inter.se, digits), nsmall = digits)
    inter.ci <- format(round(inter.ci, digits), nsmall = digits)
    inter.ci <- gsub(" ", "", inter.ci)
    slope <- x$slope.wtd
    slope.se <- sqrt(x$vcov.wtd[2,2])
    slope.ci <- c(slope - qnorm(1 - alpha/2)*slope.se, slope + qnorm(1 - alpha/2)*slope.se)
    slope <- format(round(slope, digits), nsmall = digits)
    slope.se <- format(round(slope.se, digits), nsmall = digits)
    slope.ci <- format(round(slope.ci, digits), nsmall = digits)
    slope.ci <- gsub(" ", "", slope.ci)
    DOR.meanS <- x$DOR.meanS.wtd
    DOR.meanS.ci <- x$DOR.meanS.wtd.ci
    DOR.meanS <- format(round(DOR.meanS, digits), nsmall = digits)
    DOR.meanS.ci <- format(round(DOR.meanS.ci, digits), nsmall = digits)
    DOR.meanS.ci <- gsub(" ", "", DOR.meanS.ci)
    Q <- x$Q.wtd
    Q.ci <- x$Q.wtd.ci
    Q <- format(round(Q, digits), nsmall = digits)
    Q.ci <- format(round(Q.ci, digits), nsmall = digits)
    Q.ci <- gsub(" ", "", Q.ci)
    AUC <- x$AUC.wtd
    AUC <- format(round(AUC, digits), nsmall = digits)
    cat(paste0("  intercept = ", inter, " (SE, ", inter.se, "; CI, ", inter.ci[1], " to ", inter.ci[2], ")\n"))
    cat(paste0("  slope = ", slope, " (SE, ", slope.se, "; CI, ", slope.ci[1], " to ", slope.ci[2], ")\n"))
    cat(paste0("  DOR = ", DOR.meanS, " (CI, ", DOR.meanS.ci[1], " to ", DOR.meanS.ci[2], ")\n"))
    cat(paste0("  Q point = ", Q, " (CI, ", Q.ci[1], " to ", Q.ci[2], ")\n"))
    cat(paste0("  AUC = ", AUC, "\n"))
  }

  if(is.element(x$method, c("biv.lmm", "biv.glmm"))){
    if(x$method == "biv.lmm"){
      cat("The bivariate linear mixed model is used\n")
    }
    if(x$method == "biv.glmm"){
      cat("The bivariate generalized linear mixed model is used\n")
    }

    sens.overall <- x$sens.overall
    sens.overall.ci <- x$sens.overall.ci
    sens.overall <- format(round(sens.overall, digits), nsmall = digits)
    sens.overall.ci <- format(round(sens.overall.ci, digits), nsmall = digits)
    sens.overall.ci <- gsub(" ", "", sens.overall.ci)
    spec.overall <- x$spec.overall
    spec.overall.ci <- x$spec.overall.ci
    spec.overall <- format(round(spec.overall, digits), nsmall = digits)
    spec.overall.ci <- format(round(spec.overall.ci, digits), nsmall = digits)
    spec.overall.ci <- gsub(" ", "", spec.overall.ci)
    DOR.overall <- x$DOR.overall
    DOR.overall.ci <- x$DOR.overall.ci
    DOR.overall <- format(round(DOR.overall, digits), nsmall = digits)
    DOR.overall.ci <- format(round(DOR.overall.ci, digits), nsmall = digits)
    DOR.overall.ci <- gsub(" ", "", DOR.overall.ci)
    AUC <- x$AUC
    AUC <- format(round(AUC, digits), nsmall = digits)
    mu.sens <- x$mu.sens
    mu.spec <- x$mu.spec
    sig.sens <- x$sig.sens
    sig.spec <- x$sig.spec
    rho <- x$rho
    mu.sens <- format(round(mu.sens, digits), nsmall = digits)
    mu.spec <- format(round(mu.spec, digits), nsmall = digits)
    sig.sens <- format(round(sig.sens, digits), nsmall = digits)
    sig.spec <- format(round(sig.spec, digits), nsmall = digits)
    rho <- format(round(rho, digits), nsmall = digits)
    cat(paste0("  Overall sensitivity = ", sens.overall, " (CI, ", sens.overall.ci[1], " to ", sens.overall.ci[2], ")\n"))
    cat(paste0("  Overall specificity = ", spec.overall, " (CI, ", spec.overall.ci[1], " to ", spec.overall.ci[2], ")\n"))
    cat(paste0("  Overall DOR = ", DOR.overall, " (CI, ", DOR.overall.ci[1], " to ", DOR.overall.ci[2], ")\n"))
    cat(paste0("  AUC = ", AUC, "\n"))
    cat("  Estimates of model parameters:\n")
    cat(paste0("    mu = ", mu.sens, ", sigma = ", sig.sens, " (sensitivity)\n"))
    cat(paste0("    mu = ", mu.spec, ", sigma = ", sig.spec, " (specificity)\n"))
    cat(paste0("    rho = ", rho, " (correlation coefficient)\n"))
  }
}