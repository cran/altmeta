ssfunnel.lrr <- function(y, s2, n, alpha, log.ss, p0, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...){
  plot.default(y, n, xlim = xlim, ylim = ylim,
    type = "n", log = ifelse(log.ss, "y", ""),
    xlab = xlab, ylab = ylab, ...)
  points(y, n, pch = pch, col = cols, cex = cex.pts)
  if(length(alpha) > 0){
    for(i in 1:length(alpha)){
      contour <- function(x){
        if(is.na(p0)){
          qnorm(alpha[i]/2)^2*(exp(abs(x)) - 1)/x^2
        }else{
          qnorm(alpha[i]/2)^2*(sqrt(1/p0 - 1) + sqrt(exp(-x)/p0 - 1))^2/x^2
        }
      }
      contour <- Vectorize(contour)
      if(is.na(p0)){
        curve(contour, from = -max(abs(y)) - 1, to = max(abs(y)) + 1, add = TRUE, col = cols.contour[i], lwd = lwd.contour)
      }else{
        curve(contour, from = -max(abs(y)) - 1, to = min(max(abs(y)) + 1, -log(p0)),
          add = TRUE, col = cols.contour[i], lwd = lwd.contour)
      }
    }
  }
}
