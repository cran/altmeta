ssfunnel.smd <- function(y, s2, n, alpha, log.ss, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...){
  plot.default(y, n, xlim = xlim, ylim = ylim,
    type = "n", log = ifelse(log.ss, "y", ""),
    xlab = xlab, ylab = ylab, ...)
  points(y, n, pch = pch, col = cols, cex = cex.pts)
  if(length(alpha) > 0){
    for(i in 1:length(alpha)){
      contour <- function(x){
        qnorm(alpha[i]/2)^2*(1/2 + 4/x^2)
      }
      contour <- Vectorize(contour)
      curve(contour, from = min(c(-max(abs(y)), xlim[1])) - 1, to = max(c(max(abs(y)), xlim[2])) + 1, add = TRUE, col = cols.contour[i], lwd = lwd.contour)
    }
  }
}