pb.hybrid.generic <- function(y, s2, n, data, methods, iter.resam = 1000, theo.pval = TRUE){
  if(!missing(data)){
    y <- eval(substitute(y), data, parent.frame())
    s2 <- eval(substitute(s2), data, parent.frame())
    if(!missing(n)){
      n <- eval(substitute(n), data, parent.frame())
    }
  }
  if(missing(methods)){
    methods <- c("rank", "reg", "reg.het", "skew", "skew.het", "inv.sqrt.n", "trimfill")
  }
  if(!all(is.element(methods, c("rank", "reg", "reg.het", "skew", "skew.het", "inv.sqrt.n", "trimfill")))){
    stop("incorrect input for methods.")
  }
  if(missing(n) & is.element("inv.sqrt.n", methods)){
    stop("n must be specified if 'inv.sqrt.n' is included.")
  }

  pval.rank <- pval.reg <- pval.reg.het <- pval.skew <- pval.skew.het <- pval.inv.sqrt.n <- pval.trimfill <- NA
  pval.rank.theo <- pval.reg.theo <- pval.reg.het.theo <- pval.skew.theo <- pval.skew.het.theo <- pval.inv.sqrt.n.theo <- pval.trimfill.theo <- NA
  if(is.element("rank", methods)){
    rank <- pb.rank(y, s2)
    stat.rank <- rank$stat
    if(theo.pval) pval.rank.theo <- rank$pval
  }
  if(is.element("reg", methods)){
    reg <- pb.reg(y, s2)
    stat.reg <- reg$stat
    if(theo.pval) pval.reg.theo <- reg$pval
  }
  if(is.element("reg.het", methods)){
    reg.het <- pb.reg.het(y, s2)
    stat.reg.het <- reg.het$stat
    if(theo.pval) pval.reg.het.theo <- reg.het$pval
  }
  if(is.element("skew", methods)){
    skew <- pb.skew(y, s2)
    stat.skew <- skew$stat
    if(theo.pval) pval.skew.theo <- skew$pval
  }
  if(is.element("skew.het", methods)){
    skew.het <- pb.skew.het(y, s2)
    stat.skew.het <- skew.het$stat
    if(theo.pval) pval.skew.het.theo <- skew.het$pval
  }
  if(is.element("inv.sqrt.n", methods)){
    inv.sqrt.n <- pb.inv.sqrt.n(y, s2, n)
    stat.inv.sqrt.n <- inv.sqrt.n$stat
    if(theo.pval) pval.inv.sqrt.n.theo <- inv.sqrt.n$pval
  }
  if(is.element("trimfill", methods)){
    options(warn = -1)
    rma <- rma(yi = y, vi = s2, method = "DL")
    trimfill <- pb.trimfill(rma, estimator = "R0")
    stat.trimfill <- trimfill$k0
    if(theo.pval) pval.trimfill.theo <- trimfill$pval
    options(warn = 0)
  }

  N <- length(y)
  w <- 1/s2
  theta.hat <- sum(w*y)/sum(w)
  Q <- sum(w*(y - theta.hat)^2)
  tau2.hat <- (Q - N + 1)/(sum(w) - sum(w^2)/sum(w))
  tau2.hat <- max(c(0, tau2.hat))
  w <- 1/(s2 + tau2.hat)
  theta.hat <- sum(w*y)/sum(w)
  if(all(is.na(n))) n <- rep(NA, N)

  stat.rank.resam <- stat.reg.resam <- stat.reg.het.resam <- stat.skew.resam <- stat.skew.het.resam <-
    stat.inv.sqrt.n.resam <- stat.trimfill.resam <- stat.hybrid.resam <- stat.hybrid.theo.resam <- rep(NA, iter.resam)
  pval.rank.theo.resam <- pval.reg.theo.resam <- pval.reg.het.theo.resam <- pval.skew.theo.resam <- pval.skew.het.theo.resam <-
    pval.inv.sqrt.n.theo.resam <- pval.trimfill.theo.resam <- rep(NA, iter.resam)
  for(i in 1:iter.resam){
    idx <- sample(1:N, replace = TRUE)
    s2.resam <- s2[idx]
    n.resam <- n[idx]
    y.resam <- rnorm(n = N, mean = theta.hat, sd = sqrt(s2.resam + tau2.hat))
    if(is.element("rank", methods)){
      rank.resam <- pb.rank(y.resam, s2.resam)
      stat.rank.resam[i] <- rank.resam$stat
      if(theo.pval) pval.rank.theo.resam[i] <- rank.resam$pval
    }
    if(is.element("reg", methods)){
      reg.resam <- pb.reg(y.resam, s2.resam)
      stat.reg.resam[i] <- reg.resam$stat
      if(theo.pval) pval.reg.theo.resam[i] <- reg.resam$pval
    }
    if(is.element("reg.het", methods)){
      reg.het.resam <- pb.reg.het(y.resam, s2.resam)
      stat.reg.het.resam[i] <- reg.het.resam$stat
      if(theo.pval) pval.reg.het.theo.resam[i] <- reg.het.resam$pval
    }
    if(is.element("skew", methods)){
      skew.resam <- pb.skew(y.resam, s2.resam)
      stat.skew.resam[i] <- skew.resam$stat
      if(theo.pval) pval.skew.theo.resam[i] <- skew.resam$pval
    }
    if(is.element("skew.het", methods)){
      skew.het.resam <- pb.skew.het(y.resam, s2.resam)
      stat.skew.het.resam[i] <- skew.het.resam$stat
      if(theo.pval) pval.skew.het.theo.resam[i] <- skew.het.resam$pval
    }
    if(is.element("inv.sqrt.n", methods)){
      inv.sqrt.n.resam <- pb.inv.sqrt.n(y.resam, s2.resam, n.resam)
      stat.inv.sqrt.n.resam[i] <- inv.sqrt.n.resam$stat
      if(theo.pval) pval.inv.sqrt.n.theo.resam[i] <- inv.sqrt.n.resam$pval
    }
    if(is.element("trimfill", methods)){
      options(warn = -1)
      rma.resam <- rma(yi = y.resam, vi = s2.resam, method = "DL")
      trimfill.resam <- pb.trimfill(rma.resam, estimator = "R0")
      stat.trimfill.resam[i] <- trimfill.resam$k0
      if(theo.pval) pval.trimfill.theo.resam[i] <- trimfill.resam$pval
      options(warn = 0)
    }
  }

  if(is.element("rank", methods)) pval.rank <- (sum(abs(stat.rank.resam) >= abs(stat.rank)) + 1)/(iter.resam + 1)
  if(is.element("reg", methods)) pval.reg <- (sum(abs(stat.reg.resam) >= abs(stat.reg)) + 1)/(iter.resam + 1)
  if(is.element("reg.het", methods)) pval.reg.het <- (sum(abs(stat.reg.het.resam) >= abs(stat.reg.het)) + 1)/(iter.resam + 1)
  if(is.element("skew", methods)) pval.skew <- (sum(abs(stat.skew.resam) >= abs(stat.skew)) + 1)/(iter.resam + 1)
  if(is.element("skew.het", methods)) pval.skew.het <- (sum(abs(stat.skew.het.resam) >= abs(stat.skew.het)) + 1)/(iter.resam + 1)
  if(is.element("inv.sqrt.n", methods)) pval.inv.sqrt.n <- (sum(abs(stat.inv.sqrt.n.resam) >= abs(stat.inv.sqrt.n)) + 1)/(iter.resam + 1)
  if(is.element("trimfill", methods)) pval.trimfill <- (sum(abs(stat.trimfill.resam) >= abs(stat.trimfill)) + 1)/(iter.resam + 1)

  if(length(methods) == 1){
    if(!theo.pval) out <- get(paste0("pval.", methods))
    if(theo.pval){
      out <- list(get(paste0("pval.", methods)), get(paste0("pval.", methods, ".theo")))
      names(out) <- c(paste0("pval.", methods), paste0("pval.", methods, ".theo"))
    }
  }

  if(length(methods) > 1){
    stat.hybrid <- min(c(pval.rank, pval.reg, pval.reg.het, pval.skew, pval.skew.het, pval.inv.sqrt.n, pval.trimfill), na.rm = TRUE)
    if(theo.pval) stat.hybrid.theo <- min(c(pval.rank.theo, pval.reg.theo, pval.reg.het.theo, pval.skew.theo, pval.skew.het.theo, pval.inv.sqrt.n.theo, pval.trimfill.theo), na.rm = TRUE)
    pval.rank.resam <- pval.reg.resam <- pval.reg.het.resam <- pval.skew.resam <- pval.skew.het.resam <-
      pval.inv.sqrt.n.resam <- pval.trimfill.resam <- rep(NA, iter.resam)
    for(i in 1:iter.resam){
      if(theo.pval) stat.hybrid.theo.resam[i] <- min(c(pval.rank.theo.resam[i], pval.reg.theo.resam[i], pval.reg.het.theo.resam[i], pval.skew.theo.resam[i], pval.skew.het.theo.resam[i], pval.inv.sqrt.n.theo.resam[i], pval.trimfill.theo.resam[i]), na.rm = TRUE)
      if(is.element("rank", methods)) pval.rank.resam[i] <- (sum(abs(stat.rank.resam[-i]) >= abs(stat.rank.resam[i])) + 1)/iter.resam
      if(is.element("reg", methods)) pval.reg.resam[i] <- (sum(abs(stat.reg.resam[-i]) >= abs(stat.reg.resam[i])) + 1)/iter.resam
      if(is.element("reg.het", methods)) pval.reg.het.resam[i] <- (sum(abs(stat.reg.het.resam[-i]) >= abs(stat.reg.het.resam[i])) + 1)/iter.resam
      if(is.element("skew", methods)) pval.skew.resam[i] <- (sum(abs(stat.skew.resam[-i]) >= abs(stat.skew.resam[i])) + 1)/iter.resam
      if(is.element("skew.het", methods)) pval.skew.het.resam[i] <- (sum(abs(stat.skew.het.resam[-i]) >= abs(stat.skew.het.resam[i])) + 1)/iter.resam
      if(is.element("inv.sqrt.n", methods)) pval.inv.sqrt.n.resam[i] <- (sum(abs(stat.inv.sqrt.n.resam[-i]) >= abs(stat.inv.sqrt.n.resam[i])) + 1)/iter.resam
      if(is.element("trimfill", methods)) pval.trimfill.resam[i] <- (sum(abs(stat.trimfill.resam[-i]) >= abs(stat.trimfill.resam[i])) + 1)/iter.resam
      stat.hybrid.resam[i] <- min(c(pval.rank.resam[i], pval.reg.resam[i], pval.reg.het.resam[i], pval.skew.resam[i], pval.skew.het.resam[i], pval.inv.sqrt.n.resam[i], pval.trimfill.resam[i]), na.rm = TRUE)
    }
    pval.hybrid <- (sum(stat.hybrid.resam <= stat.hybrid) + 1)/(iter.resam + 1)
    if(!theo.pval){
      out <- list(pval.rank = pval.rank, pval.reg = pval.reg, pval.reg.het = pval.reg.het, pval.skew = pval.skew, pval.skew.het = pval.skew.het,
        pval.inv.sqrt.n = pval.inv.sqrt.n, pval.trimfill = pval.trimfill, pval.hybrid = pval.hybrid)
    }
    if(theo.pval){
      pval.hybrid.theo <- (sum(stat.hybrid.theo.resam <= stat.hybrid.theo) + 1)/(iter.resam + 1)
      out <- list(pval.rank = pval.rank, pval.rank.theo = pval.rank.theo, pval.reg = pval.reg, pval.reg.theo = pval.reg.theo, pval.reg.het = pval.reg.het, pval.reg.het.theo = pval.reg.het.theo,
        pval.skew = pval.skew, pval.skew.theo = pval.skew.theo, pval.skew.het = pval.skew.het, pval.skew.het.theo = pval.skew.het.theo, pval.inv.sqrt.n = pval.inv.sqrt.n, pval.inv.sqrt.n.theo = pval.inv.sqrt.n.theo,
        pval.trimfill = pval.trimfill, pval.trimfill.theo = pval.trimfill.theo, pval.hybrid = pval.hybrid, pval.hybrid.theo = pval.hybrid.theo)
    }
    na.test <- which(is.na(out))
    if(length(na.test) > 0) out <- out[-na.test]
  }

  return(out)
}