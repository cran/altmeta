ssfunnel.rd <- function(y, s2, n, alpha, log.ss, p0, xlim, ylim, xlab, ylab, cols.contour, cols, cex.pts, lwd.contour, pch, ...){
  plot(y, n, xlim = xlim, ylim = ylim,
    type = "n", log = ifelse(log.ss, "y", ""),
    xlab = xlab, ylab = ylab, ...)
  points(y, n, pch = pch, col = cols, cex = cex.pts)
  if(length(alpha) > 0){
    for(i in 1:length(alpha)){
      contour <- function(x){
        if(is.na(p0)){
          qnorm(alpha[i]/2)^2*(1 - abs(x))/abs(x)
        }else{
          qnorm(alpha[i]/2)^2*(sqrt(p0*(1 - p0)) + sqrt((p0 + x)*(1 - p0 - x)))^2/x^2
        }
      }
      contour <- Vectorize(contour)
      if(is.na(p0)){
        curve(contour, from = -1, to = 1, add = TRUE, col = cols.contour[i], lwd = lwd.contour)
      }else{
        curve(contour, from = -p0, to = 1 - p0,
          add = TRUE, col = cols.contour[i], lwd = lwd.contour)
      }
    }
  }
}
