ssfunnel <- function(y, s2, n, data, type, alpha = c(0.1, 0.05, 0.01, 0.001), log.ss = FALSE, sigma, p0, xlim, ylim, xlab, ylab,
  cols.contour, col.mostsig, cex.pts, lwd.contour, pch, x.legend, y.legend, cex.legend, bg.legend, ...){
  if(!missing(data)){
    y <- eval(substitute(y), data, parent.frame())
    s2 <- eval(substitute(s2), data, parent.frame())
    n <- eval(substitute(n), data, parent.frame())
  }
  if(missing(y)) stop("the observed effect sizes are required.")
  if(missing(s2)) stop("the within-study variances are required.")
  if(missing(n)) stop("the sample sizes are required.")
  if(missing(type)) stop("the data type needs to be specified.")
  if(!is.element(type, c("md", "smd", "lor", "lrr", "rd"))) stop("the data type can be MD, SMD, LOR, LRR, or RD.")
  if(!is.null(alpha)){
    alpha <- sort(alpha, decreasing = TRUE)
    if(all(alpha != 0)) alpha <- c(alpha, 0)
  }
  if(any(alpha < 0) | any(alpha > 1)) stop("the significance levels should be between 0 and 1.")
  if(missing(p0)){
    p0 <- NA

  }else{
    if(p0 < 0 | p0 > 1) stop("p0 must be between 0 and 1.")
  }
  if(missing(sigma) & type == "md") stop("the standard deviation sigma must be specified for mean difference.")
  if(missing(xlim)) xlim <- c(-max(abs(y)), max(abs(y)))
  if(missing(ylim)) ylim <- c(1, max(n))

  if(missing(xlab)){
    if(type == "md") xlab <- "Mean difference"
    if(type == "smd") xlab <- "Standardized mean difference"
    if(type == "lor") xlab <- "Log odds ratio"
    if(type == "lrr") xlab <- "Log relative risk"
    if(type == "rd") xlab <- "Risk difference"
  }
  if(missing(ylab)) ylab <- "Sample size"

  pval <- 2*pnorm(-abs(y)/sqrt(s2))
  cols <- rep(NA, length(y))
  if(missing(cols.contour)){
    if(length(alpha) > 0){
      cols.contour <- rep(NA, length(alpha))
      cols[pval >= alpha[1]] <- cols.contour[1] <- "gray80"
    }else{
      cols <- rep("black", length(y))
    }
    if(length(alpha) > 1) cols[pval < alpha[1] & pval >= alpha[2]] <- cols.contour[2] <- "gray50"
    if(length(alpha) > 2){
      for(i in 3:length(alpha)){
        cols[pval < alpha[i - 1] & pval >= alpha[i]] <- cols.contour[i] <- paste0("gray", 10*max(c(0, 5 - (i - 2))))
      }
    }
  }else{
    if(missing(col.mostsig)) col.mostsig <- "gray20"
    cols.contour <- c(cols.contour, col.mostsig)
    if(length(alpha) != length(cols.contour)) stop("the length of cols.contour is not equal to the length of alpha.")
    if(length(alpha) > 0){
      cols[pval >= alpha[1]] <- cols.contour[1]
    }else{
      cols <- rep("black", length(y))
    }
    if(length(alpha) > 1) cols[pval < alpha[1] & pval >= alpha[2]] <- cols.contour[2]
    if(length(alpha) > 2){
      for(i in 3:length(alpha)){
        cols[pval < alpha[i - 1] & pval >= alpha[i]] <- cols.contour[i]
      }
    }
  }
  cols.contour <- cols.contour[-length(cols.contour)]
  alpha <- alpha[-length(alpha)]

  if(missing(cex.pts)) cex.pts <- 1
  if(missing(lwd.contour)) lwd.contour <- 1
  if(missing(pch)) pch <- 19

  if(type == "md") ssfunnel.md(y, s2, n, alpha, log.ss, sigma, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...)
  if(type == "smd") ssfunnel.smd(y, s2, n, alpha, log.ss, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...)
  if(type == "lor") ssfunnel.lor(y, s2, n, alpha, log.ss, p0, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...)
  if(type == "lrr") ssfunnel.lrr(y, s2, n, alpha, log.ss, p0, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...)
  if(type == "rd") ssfunnel.rd(y, s2, n, alpha, log.ss, p0, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...)

  if(missing(x.legend)) x.legend <- "topleft"
  if(missing(y.legend)) y.legned <- NULL
  if(missing(cex.legend)) cex.legend <- 1
  if(missing(bg.legend)) bg.legend <- "white"

  alpha.legend <- alpha
  legend(x = x.legend, y = y.legend, ncol = 1,
    legend = sapply(1:length(alpha), function(i) as.expression(substitute(alpha == A, list(A = alpha.legend[i])))),
    col = cols.contour, lwd = lwd.contour, title = NULL, cex = cex.legend, bg = bg.legend)
}
