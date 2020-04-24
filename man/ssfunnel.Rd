\name{ssfunnel}
\alias{ssfunnel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contour-Enhanced Sample-Size-Based Funnel Plot
}
\description{
Generates contour-enhanced sample-size-based funnel plot for a meta-analysis of mean differences, standardized mean differences, (log) odds ratios, (log) relative risks, or risk differences.
}
\usage{
ssfunnel(y, s2, n, data, type, alpha = c(0.1, 0.05, 0.01, 0.001),
         log.ss = FALSE, sigma, p0, xlim, ylim, xlab, ylab,
         cols.contour, col.mostsig, cex.pts, lwd.contour, pch,
         x.legend, y.legend, cex.legend, bg.legend, ...)
}
\arguments{
  \item{y}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the observed effect sizes in the collected studies.
}
  \item{s2}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the within-study variances.
}
  \item{n}{
  a numeric vector or the corresponding column name in the argument \code{data}, indicating the study-specific total sample sizes.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{y}, \code{s2}, and \code{n}, should be specified as their corresponding column names in \code{data}.
}
  \item{type}{
  a character string indicating the type of effect size, which should be one of \code{"md"} (mean difference), \code{"smd"} (standardized mean difference), \code{"lor"} (log odds ratio), \code{"lrr"} (log relative risk), and \code{"rd"} (risk difference).
}
  \item{alpha}{
  a numeric vector indicating the significance levels to be presented in the sample-size-based funnel plot.
}
  \item{log.ss}{
  a logical value indicating whether sample sizes are plotted on a logarithmic scale (\code{TRUE}) or not (\code{FALSE}, the default).
}
  \item{sigma}{
  a positive numeric value that is required for the mean difference (\code{type} = \code{"md"}), indicating a rough estimate of the common standard deviation of the samples' continuous outcomes in the two groups across studies. It is not used for other effect size types.
}
  \item{p0}{
  an optional numeric value indicating a rough estimate of the common event rate in the control group across studies. It is only used for the (log) odds ratio, (log) relative risk, and risk difference.
}
  \item{xlim}{
  the x limits \code{c(x1, x2)} of the plot.
}
  \item{ylim}{
  the y limits \code{c(y1, y2)} of the plot.
}
  \item{xlab}{
  a label for the x axis.
}
  \item{ylab}{
  a label for the y axis.
}
  \item{cols.contour}{
  a vector of character strings; they indicate colors of the contours to be presented in the sample-size-based funnel plot, and correspond to the significance levels specified in the argument \code{alpha}.
}
  \item{col.mostsig}{
  a character string indicating the color for the most significant result among the studies in the meta-analysis.
}
  \item{cex.pts}{
  the size of the points.
}
  \item{lwd.contour}{
  the width of the contours.
}
  \item{pch}{
  the symbol of the points.
}
  \item{x.legend}{
  the x co-ordinate or a keyword, such as \code{"topleft"} (the default), to be used to position the legend. It is passed to \code{\link[graphics]{legend}}.
}
  \item{y.legend}{
  the y co-ordinate to be used to position the legend (the default is \code{NULL}).
}
  \item{cex.legend}{
  the size of legend text.
}
  \item{bg.legend}{
  the background color for the legend box.
}
  \item{\dots}{
  other arguments that can be passed to \code{\link[graphics]{plot.default}}.
}
}
\details{
  A contour-enhanced sample-size-based funnel plot is generated; it presents study-specific total sample sizes against the corresponding effect size estimates. It is helpful to avoid the confounding effect caused by the intrinsic association between effect size estimates and standard errors in the conventional standard-error-based funnel plot. See details of the derivations of the contours in Lin (2019).
}
\value{
None.
}
\references{
Lin L (2019). "Graphical augmentations to sample-size-based funnel plot in meta-analysis." \emph{Research Synthesis Methods}, \bold{10}(3), 376--388. <\doi{10.1002/jrsm.1340}>

Peters JL, Sutton AJ, Jones DR, Abrams KR, Rushton L (2006). "Comparison of two methods to detect publication bias in meta-analysis." \emph{JAMA}, \bold{295}(6), 676--680. <\doi{10.1001/jama.295.6.676}>
}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## mean difference
data("dat.annane")
# descriptive statistics for sigma (continuous outcomes' standard deviation)
quantile(sqrt(dat.annane$s2/(1/dat.annane$n1 + 1/dat.annane$n2)),
  probs = c(0, 0.25, 0.5, 0.75, 1))
# based on sigma = 8
ssfunnel(y, s2, n, data = dat.annane, type = "md",
  alpha = c(0.1, 0.05, 0.01, 0.001), sigma = 8)
# sample sizes presented on a logarithmic scale with plot title
ssfunnel(y, s2, n, data = dat.annane, type = "md",
  alpha = c(0.1, 0.05, 0.01, 0.001), sigma = 8, log.ss = TRUE,
  main = "Contour-enhanced sample-size-based funnel plot")
# based on sigma = 17, with specified x and y limits
ssfunnel(y, s2, n, data = dat.annane, type = "md",
  xlim = c(-15, 15), ylim = c(30, 500),
  alpha = c(0.1, 0.05, 0.01, 0.001), sigma = 17, log.ss = TRUE)
# based on sigma = 20
ssfunnel(y, s2, n, data = dat.annane, type = "md",
  xlim = c(-15, 15), ylim = c(30, 500),
  alpha = c(0.1, 0.05, 0.01, 0.001), sigma = 20, log.ss = TRUE)

## standardized mean difference
data("dat.barlow")
ssfunnel(y, s2, n, data = dat.barlow, type = "smd",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 1))

## log odds ratio
data("dat.butters")
ssfunnel(y, s2, n, data = dat.butters, type = "lor",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 1.5))
# use different colors for contours
ssfunnel(y, s2, n, data = dat.butters, type = "lor",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 1.5),
  cols.contour = c("blue", "green", "yellow", "red"), col.mostsig = "black")
# based on p0 = 0.3 (common event rate in the control group across studies)
ssfunnel(y, s2, n, data = dat.butters, type = "lor",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 1.5), p0 = 0.3)
# based on p0 = 0.5
ssfunnel(y, s2, n, data = dat.butters, type = "lor",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 1.5), p0 = 0.5)

## log relative risk
data("dat.williams")
ssfunnel(y, s2, n, data = dat.williams, type = "lrr",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-1.5, 2.5))
# based on p0 = 0.2
ssfunnel(y, s2, n, data = dat.williams, type = "lrr",
  alpha = c(0.1, 0.05, 0.01, 0.001), p0 = 0.2, xlim = c(-1.5, 2.5))
# based on p0 = 0.3
ssfunnel(y, s2, n, data = dat.williams, type = "lrr",
  alpha = c(0.1, 0.05, 0.01, 0.001), p0 = 0.3, xlim = c(-1.5, 2.5))

## risk difference
data("dat.kaner")
ssfunnel(y, s2, n, data = dat.kaner, type = "rd",
  alpha = c(0.1, 0.05, 0.01, 0.001), xlim = c(-0.5, 0.5))
# based on p0 = 0.1
ssfunnel(y, s2, n, data = dat.kaner, type = "rd",
  alpha = c(0.1, 0.05, 0.01, 0.001), p0 = 0.1, xlim = c(-0.5, 0.5))
# based on p0 = 0.4
ssfunnel(y, s2, n, data = dat.kaner, type = "rd",
  alpha = c(0.1, 0.05, 0.01, 0.001), p0 = 0.4, xlim = c(-0.5, 0.5))
}
\keyword{publication bias}
\keyword{plot}