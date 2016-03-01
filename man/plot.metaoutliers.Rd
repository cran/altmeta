\name{plot.metaoutliers}
\alias{plot.metaoutliers}

\title{
Standardized Residual Plot for Outliers Diagnostics
}
\description{
Draws a plot showing study-specific standardized residuals.
}
\usage{
\method{plot}{metaoutliers}(x, xtick.cex = 1, ytick.cex = 0.5, ...)
}

\arguments{
  \item{x}{
  an object created by the function \code{\link{metaoutliers}()}.
}
  \item{xtick.cex}{
  a numerical value indicating the magnification to be used for ticks on x-axis.
}
  \item{ytick.cex}{
  a numerical value indicating the magnification to be used for ticks on y-axis.
}
  \item{...}{
  Other arguments can be passed to \code{plot()} function.
}
}

\seealso{
\code{\link{metaoutliers}}
}
\examples{
data("aex")
attach(aex)
out.aex <- metaoutliers(y, s2, model = "FE")
detach(aex)
plot(out.aex)

data("hipfrac")
attach(hipfrac)
out.hipfrac <- metaoutliers(y, s2, model = "RE")
detach(hipfrac)
plot(out.hipfrac)
}
