pb.hybrid.binary <- function(n00, n01, n10, n11, data, methods, iter.resam = 1000, theo.pval = TRUE){
  if(!missing(data)){
    n00 <- eval(substitute(n00), data, parent.frame())
    n01 <- eval(substitute(n01), data, parent.frame())
    n10 <- eval(substitute(n10), data, parent.frame())
    n11 <- eval(substitute(n11), data, parent.frame())
  }
  if(missing(methods)){
    methods <- c("rank", "reg", "reg.het", "skew", "skew.het", "inv.sqrt.n", "trimfill",
      "n", "inv.n", "as.rank", "as.reg", "as.reg.het", "smoothed", "smoothed.het", "score", "count")
  }
  if(!all(is.element(methods, c("rank", "reg", "reg.het", "skew", "skew.het", "inv.sqrt.n", "trimfill",
    "n", "inv.n", "as.rank", "as.reg", "as.reg.het", "smoothed", "smoothed.het", "score", "count")))){
    stop("incorrect input for methods.")
  }

  pval.rank <- pval.reg <- pval.reg.het <- pval.skew <- pval.skew.het <- pval.inv.sqrt.n <- pval.trimfill <-
    pval.n <- pval.inv.n <- pval.as.rank <- pval.as.reg <- pval.as.reg.het <-
    pval.smoothed <- pval.smoothed.het <- pval.score <- pval.count <- NA
  pval.rank.theo <- pval.reg.theo <- pval.reg.het.theo <- pval.skew.theo <- pval.skew.het.theo <- pval.inv.sqrt.n.theo <- pval.trimfill.theo <-
    pval.n.theo <- pval.inv.n.theo <- pval.as.rank.theo <- pval.as.reg.theo <- pval.as.reg.het.theo <-
    pval.smoothed.theo <- pval.smoothed.het.theo <- pval.score.theo <- pval.count.theo <- NA

  n00.ori <- n00
  n01.ori <- n01
  n10.ori <- n10
  n11.ori <- n11

  counts <- check.counts(n00, n01, n10, n11)
  n00 <- counts$n00
  n01 <- counts$n01
  n10 <- counts$n10
  n11 <- counts$n11

  y <- log(n11/n10*n00/n01)
  s2 <- 1/n00 + 1/n01 + 1/n10 + 1/n11
  n <- n00 + n01 + n10 + n11
  pi0 <- n01/(n00 + n01)
  n0Sum <- n00 + n01
  n1Sum <- n10 + n11

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
  if(is.element("n", methods)){
    pbn <- pb.n(y = y, n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.n <- pbn$stat
    if(theo.pval) pval.n.theo <- pbn$pval
  }
  if(is.element("inv.n", methods)){
    inv.n <- pb.inv.n(y = y, n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.inv.n <- inv.n$stat
    if(theo.pval) pval.inv.n.theo <- inv.n$pval
  }
  if(is.element("as.rank", methods)){
    as.rank <- pb.as.rank(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.as.rank <- as.rank$stat
    if(theo.pval) pval.as.rank.theo <- as.rank$pval
  }
  if(is.element("as.reg", methods)){
    as.reg <- pb.as.reg(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.as.reg <- as.reg$stat
    if(theo.pval) pval.as.reg.theo <- as.reg$pval
  }
  if(is.element("as.reg.het", methods)){
    as.reg.het <- pb.as.reg.het(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.as.reg.het <- as.reg.het$stat
    if(theo.pval) pval.as.reg.het.theo <- as.reg.het$pval
  }
  if(is.element("smoothed", methods)){
    smoothed <- pb.smoothed(y = y, n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.smoothed <- smoothed$stat
    if(theo.pval) pval.smoothed.theo <- smoothed$pval
  }
  if(is.element("smoothed.het", methods)){
    smoothed.het <- pb.smoothed.het(y = y, n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.smoothed.het <- smoothed.het$stat
    if(theo.pval) pval.smoothed.het.theo <- smoothed.het$pval
  }
  if(is.element("score", methods)){
    score <- pb.score(n00 = n00, n01 = n01, n10 = n10, n11 = n11)
    stat.score <- score$stat
    if(theo.pval) pval.score.theo <- score$pval
  }
  if(is.element("count", methods)){
    count <- pb.count(n00 = n00.ori, n01 = n01.ori, n10 = n10.ori, n11 = n11.ori)
    stat.count <- count$stat
    if(theo.pval) pval.count.theo <- count$pval
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
    stat.inv.sqrt.n.resam <- stat.trimfill.resam <-
    stat.n.resam <- stat.inv.n.resam <- stat.as.rank.resam <- stat.as.reg.resam <- stat.as.reg.het.resam <-
    stat.smoothed.resam <- stat.smoothed.het.resam <- stat.score.resam <- stat.count.resam <- stat.hybrid.resam <- stat.hybrid.theo.resam <- rep(NA, iter.resam)
  pval.rank.theo.resam <- pval.reg.theo.resam <- pval.reg.het.theo.resam <- pval.skew.theo.resam <- pval.skew.het.theo.resam <- pval.inv.sqrt.n.theo.resam <- pval.trimfill.theo.resam <-
    pval.n.theo.resam <- pval.inv.n.theo.resam <- pval.as.rank.theo.resam <- pval.as.reg.theo.resam <- pval.as.reg.het.theo.resam <-
    pval.smoothed.theo.resam <- pval.smoothed.het.theo.resam <- pval.score.theo.resam <- pval.count.theo.resam <- rep(NA, iter.resam)
  for(i in 1:iter.resam){
    idx <- sample(1:N, replace = TRUE)
    n0Sum.resam <- n0Sum[idx]
    n1Sum.resam <- n1Sum[idx]
    pi0.resam <- pi0[idx]
    s2.resam <- s2[idx]
    theta.resam <- rnorm(n = N, mean = theta.hat, sd = sqrt(s2.resam + tau2.hat))
    n00.resam <- n01.resam <- n10.resam <- n11.resam <- rep(NA, N)
    for(j in 1:N){
      counts.resam <- find.counts(n0. = n0Sum.resam[j], n1. = n1Sum.resam[j], lor = theta.resam[j], lor.var = s2.resam[j], p0.ori = pi0.resam[j])
      n00.resam[j] <- counts.resam$n00
      n01.resam[j] <- counts.resam$n01
      n10.resam[j] <- counts.resam$n10
      n11.resam[j] <- counts.resam$n11
    }
    n00.ori.resam <- n00.resam
    n00.ori.resam <- round(n00.ori.resam)
    n01.ori.resam <- n01.resam
    n01.ori.resam <- round(n01.ori.resam)
    n10.ori.resam <- n10.resam
    n10.ori.resam <- round(n10.ori.resam)
    n11.ori.resam <- n11.resam
    n11.ori.resam <- round(n11.ori.resam)
    counts.resam <- check.counts(n00.resam, n01.resam, n10.resam, n11.resam)
    n00.resam <- counts.resam$n00
    n01.resam <- counts.resam$n01
    n10.resam <- counts.resam$n10
    n11.resam <- counts.resam$n11
    y.resam <- log(n11.resam/n10.resam*n00.resam/n01.resam)
    s2.resam <- 1/n00.resam + 1/n01.resam + 1/n10.resam + 1/n11.resam
    n.resam <- n00.resam + n01.resam + n10.resam + n11.resam

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
    if(is.element("n", methods)){
      pbn.resam <- pb.n(y = y.resam, n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.n.resam[i] <- pbn.resam$stat
      if(theo.pval) pval.n.theo.resam[i] <- pbn.resam$pval
    }
    if(is.element("inv.n", methods)){
      inv.n.resam <- pb.inv.n(y = y.resam, n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.inv.n.resam[i] <- inv.n.resam$stat
      if(theo.pval) pval.inv.n.theo.resam[i] <- inv.n.resam$pval
    }
    if(is.element("as.rank", methods)){
      as.rank.resam <- pb.as.rank(n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.as.rank.resam[i] <- as.rank.resam$stat
      if(theo.pval) pval.as.rank.theo.resam[i] <- as.rank.resam$pval
    }
    if(is.element("as.reg", methods)){
      as.reg.resam <- pb.as.reg(n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.as.reg.resam[i] <- as.reg.resam$stat
      if(theo.pval) pval.as.reg.theo.resam[i] <- as.reg.resam$pval
    }
    if(is.element("as.reg.het", methods)){
      as.reg.het.resam <- pb.as.reg.het(n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.as.reg.het.resam[i] <- as.reg.het.resam$stat
      if(theo.pval) pval.as.reg.het.theo.resam[i] <- as.reg.het.resam$pval
    }
    if(is.element("smoothed", methods)){
      smoothed.resam <- pb.smoothed(y = y.resam, n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.smoothed.resam[i] <- smoothed.resam$stat
      if(theo.pval) pval.smoothed.theo.resam[i] <- smoothed.resam$pval
    }
    if(is.element("smoothed.het", methods)){
      smoothed.het.resam <- pb.smoothed.het(y = y.resam, n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.smoothed.het.resam[i] <- smoothed.het.resam$stat
      if(theo.pval) pval.smoothed.het.theo.resam[i] <- smoothed.het.resam$pval
    }
    if(is.element("score", methods)){
      score.resam <- pb.score(n00 = n00.resam, n01 = n01.resam, n10 = n10.resam, n11 = n11.resam)
      stat.score.resam[i] <- score.resam$stat
      if(theo.pval) pval.score.theo.resam[i] <- score.resam$pval
    }
    if(is.element("count", methods)){
      count.resam <- pb.count(n00 = n00.ori.resam, n01 = n01.ori.resam, n10 = n10.ori.resam, n11 = n11.ori.resam)
      stat.count.resam[i] <- count.resam$stat
      if(theo.pval) pval.count.theo.resam[i] <- count.resam$pval
    }
  }

  if(is.element("rank", methods)) pval.rank <- (sum(abs(stat.rank.resam) >= abs(stat.rank)) + 1)/(iter.resam + 1)
  if(is.element("reg", methods)) pval.reg <- (sum(abs(stat.reg.resam) >= abs(stat.reg)) + 1)/(iter.resam + 1)
  if(is.element("reg.het", methods)) pval.reg.het <- (sum(abs(stat.reg.het.resam) >= abs(stat.reg.het)) + 1)/(iter.resam + 1)
  if(is.element("skew", methods)) pval.skew <- (sum(abs(stat.skew.resam) >= abs(stat.skew)) + 1)/(iter.resam + 1)
  if(is.element("skew.het", methods)) pval.skew.het <- (sum(abs(stat.skew.het.resam) >= abs(stat.skew.het)) + 1)/(iter.resam + 1)
  if(is.element("inv.sqrt.n", methods)) pval.inv.sqrt.n <- (sum(abs(stat.inv.sqrt.n.resam) >= abs(stat.inv.sqrt.n)) + 1)/(iter.resam + 1)
  if(is.element("trimfill", methods)) pval.trimfill <- (sum(abs(stat.trimfill.resam) >= abs(stat.trimfill)) + 1)/(iter.resam + 1)
  if(is.element("n", methods)) pval.n <- (sum(abs(stat.n.resam) >= abs(stat.n)) + 1)/(iter.resam + 1)
  if(is.element("inv.n", methods)) pval.inv.n <- (sum(abs(stat.inv.n.resam) >= abs(stat.inv.n)) + 1)/(iter.resam + 1)
  if(is.element("as.rank", methods)) pval.as.rank <- (sum(abs(stat.as.rank.resam) >= abs(stat.as.rank)) + 1)/(iter.resam + 1)
  if(is.element("as.reg", methods)) pval.as.reg <- (sum(abs(stat.as.reg.resam) >= abs(stat.as.reg)) + 1)/(iter.resam + 1)
  if(is.element("as.reg.het", methods)) pval.as.reg.het <- (sum(abs(stat.as.reg.het.resam) >= abs(stat.as.reg.het)) + 1)/(iter.resam + 1)
  if(is.element("smoothed", methods)) pval.smoothed <- (sum(abs(stat.smoothed.resam) >= abs(stat.smoothed)) + 1)/(iter.resam + 1)
  if(is.element("smoothed.het", methods)) pval.smoothed.het <- (sum(abs(stat.smoothed.het.resam) >= abs(stat.smoothed.het)) + 1)/(iter.resam + 1)
  if(is.element("score", methods)) pval.score <- (sum(abs(stat.score.resam) >= abs(stat.score)) + 1)/(iter.resam + 1)
  if(is.element("count", methods)) pval.count <- (sum(abs(stat.count.resam) >= abs(stat.count)) + 1)/(iter.resam + 1)

  if(length(methods) == 1){
    if(!theo.pval) out <- get(paste0("pval.", methods))
    if(theo.pval){
      out <- list(get(paste0("pval.", methods)), get(paste0("pval.", methods, ".theo")))
      names(out) <- c(paste0("pval.", methods), paste0("pval.", methods, ".theo"))
    }
  }

  if(length(methods) > 1){
    stat.hybrid <- min(c(pval.rank, pval.reg, pval.reg.het, pval.skew, pval.skew.het, pval.inv.sqrt.n, pval.trimfill,
      pval.n, pval.inv.n, pval.as.rank, pval.as.reg, pval.as.reg.het, pval.smoothed, pval.smoothed.het, pval.score, pval.count), na.rm = TRUE)
    if(theo.pval) stat.hybrid.theo <- min(c(pval.rank.theo, pval.reg.theo, pval.reg.het.theo, pval.skew.theo, pval.skew.het.theo, pval.inv.sqrt.n.theo, pval.trimfill.theo,
      pval.n.theo, pval.inv.n.theo, pval.as.rank.theo, pval.as.reg.theo, pval.as.reg.het.theo, pval.smoothed.theo, pval.smoothed.het.theo, pval.score.theo, pval.count.theo), na.rm = TRUE)
    pval.rank.resam <- pval.reg.resam <- pval.reg.het.resam <- pval.skew.resam <- pval.skew.het.resam <-
      pval.inv.sqrt.n.resam <- pval.trimfill.resam <-
      pval.n.resam <- pval.inv.n.resam <- pval.as.rank.resam <- pval.as.reg.resam <- pval.as.reg.het.resam <-
      pval.smoothed.resam <- pval.smoothed.het.resam <- pval.score.resam <- pval.count.resam <- rep(NA, iter.resam)
    for(i in 1:iter.resam){
      if(theo.pval) stat.hybrid.theo.resam[i] <- min(c(pval.rank.theo.resam[i], pval.reg.theo.resam[i], pval.reg.het.theo.resam[i], pval.skew.theo.resam[i], pval.skew.het.theo.resam[i], pval.inv.sqrt.n.theo.resam[i], pval.trimfill.theo.resam[i],
        pval.n.theo.resam[i], pval.inv.n.theo.resam[i], pval.as.rank.theo.resam[i], pval.as.reg.theo.resam[i], pval.as.reg.het.theo.resam[i], pval.smoothed.theo.resam[i], pval.smoothed.het.theo.resam[i], pval.score.theo.resam[i], pval.count.theo.resam[i]), na.rm = TRUE)
      if(is.element("rank", methods)) pval.rank.resam[i] <- (sum(abs(stat.rank.resam[-i]) >= abs(stat.rank.resam[i])) + 1)/iter.resam
      if(is.element("reg", methods)) pval.reg.resam[i] <- (sum(abs(stat.reg.resam[-i]) >= abs(stat.reg.resam[i])) + 1)/iter.resam
      if(is.element("reg.het", methods)) pval.reg.het.resam[i] <- (sum(abs(stat.reg.het.resam[-i]) >= abs(stat.reg.het.resam[i])) + 1)/iter.resam
      if(is.element("skew", methods)) pval.skew.resam[i] <- (sum(abs(stat.skew.resam[-i]) >= abs(stat.skew.resam[i])) + 1)/iter.resam
      if(is.element("skew.het", methods)) pval.skew.het.resam[i] <- (sum(abs(stat.skew.het.resam[-i]) >= abs(stat.skew.het.resam[i])) + 1)/iter.resam
      if(is.element("inv.sqrt.n", methods)) pval.inv.sqrt.n.resam[i] <- (sum(abs(stat.inv.sqrt.n.resam[-i]) >= abs(stat.inv.sqrt.n.resam[i])) + 1)/iter.resam
      if(is.element("trimfill", methods)) pval.trimfill.resam[i] <- (sum(abs(stat.trimfill.resam[-i]) >= abs(stat.trimfill.resam[i])) + 1)/iter.resam
      if(is.element("n", methods)) pval.n.resam[i] <- (sum(abs(stat.n.resam[-i]) >= abs(stat.n.resam[i])) + 1)/iter.resam
      if(is.element("inv.n", methods)) pval.inv.n.resam[i] <- (sum(abs(stat.inv.n.resam[-i]) >= abs(stat.inv.n.resam[i])) + 1)/iter.resam
      if(is.element("as.rank", methods)) pval.as.rank.resam[i] <- (sum(abs(stat.as.rank.resam[-i]) >= abs(stat.as.rank.resam[i])) + 1)/iter.resam
      if(is.element("as.reg", methods)) pval.as.reg.resam[i] <- (sum(abs(stat.as.reg.resam[-i]) >= abs(stat.as.reg.resam[i])) + 1)/iter.resam
      if(is.element("as.reg.het", methods)) pval.as.reg.het.resam[i] <- (sum(abs(stat.as.reg.het.resam[-i]) >= abs(stat.as.reg.het.resam[i])) + 1)/iter.resam
      if(is.element("smoothed", methods)) pval.smoothed.resam[i] <- (sum(abs(stat.smoothed.resam[-i]) >= abs(stat.smoothed.resam[i])) + 1)/iter.resam
      if(is.element("smoothed.het", methods)) pval.smoothed.het.resam[i] <- (sum(abs(stat.smoothed.het.resam[-i]) >= abs(stat.smoothed.het.resam[i])) + 1)/iter.resam
      if(is.element("score", methods)) pval.score.resam[i] <- (sum(abs(stat.score.resam[-i]) >= abs(stat.score.resam[i])) + 1)/iter.resam
      if(is.element("count", methods)) pval.count.resam[i] <- (sum(abs(stat.count.resam[-i]) >= abs(stat.count.resam[i])) + 1)/iter.resam
      stat.hybrid.resam[i] <- min(c(pval.rank.resam[i], pval.reg.resam[i], pval.reg.het.resam[i], pval.skew.resam[i], pval.skew.het.resam[i], pval.inv.sqrt.n.resam[i], pval.trimfill.resam[i],
        pval.n.resam[i], pval.inv.n.resam[i], pval.as.rank.resam[i], pval.as.reg.resam[i], pval.as.reg.het.resam[i], pval.smoothed.resam[i], pval.smoothed.het.resam[i], pval.score.resam[i], pval.count.resam[i]), na.rm = TRUE)
    }
    pval.hybrid <- (sum(stat.hybrid.resam <= stat.hybrid) + 1)/(iter.resam + 1)
    if(!theo.pval){
      out <- list(pval.rank = pval.rank, pval.reg = pval.reg, pval.reg.het = pval.reg.het, pval.skew = pval.skew, pval.skew.het = pval.skew.het,
        pval.inv.sqrt.n = pval.inv.sqrt.n, pval.trimfill = pval.trimfill,
        pval.n = pval.n, pval.inv.n = pval.inv.n, pval.as.rank = pval.as.rank, pval.as.reg = pval.as.reg, pval.as.reg.het = pval.as.reg.het,
        pval.smoothed = pval.smoothed, pval.smoothed.het = pval.smoothed.het, pval.score = pval.score, pval.count = pval.count,
        pval.hybrid = pval.hybrid)
    }
    if(theo.pval){
      pval.hybrid.theo <- (sum(stat.hybrid.theo.resam <= stat.hybrid.theo) + 1)/(iter.resam + 1)
      out <- list(pval.rank = pval.rank, pval.rank.theo = pval.rank.theo, pval.reg = pval.reg, pval.reg.theo = pval.reg.theo, pval.reg.het = pval.reg.het, pval.reg.het.theo = pval.reg.het.theo, pval.skew = pval.skew, pval.skew.theo = pval.skew.theo, pval.skew.het = pval.skew.het, pval.skew.het.theo = pval.skew.het.theo,
        pval.inv.sqrt.n = pval.inv.sqrt.n, pval.inv.sqrt.n.theo = pval.inv.sqrt.n.theo, pval.trimfill = pval.trimfill, pval.trimfill.theo = pval.trimfill.theo,
        pval.n = pval.n, pval.n.theo = pval.n.theo, pval.inv.n = pval.inv.n, pval.inv.n.theo = pval.inv.n.theo, pval.as.rank = pval.as.rank, pval.as.rank.theo = pval.as.rank.theo, pval.as.reg = pval.as.reg, pval.as.reg.theo = pval.as.reg.theo, pval.as.reg.het = pval.as.reg.het, pval.as.reg.het.theo = pval.as.reg.het.theo,
        pval.smoothed = pval.smoothed, pval.smoothed.theo = pval.smoothed.theo, pval.smoothed.het = pval.smoothed.het, pval.smoothed.het.theo = pval.smoothed.het.theo, pval.score = pval.score, pval.score.theo = pval.score.theo, pval.count = pval.count, pval.count.theo = pval.count.theo,
        pval.hybrid = pval.hybrid, pval.hybrid.theo = pval.hybrid.theo)
    }
    na.test <- which(is.na(out))
    if(length(na.test) > 0) out <- out[-na.test]
  }

  return(out)
}