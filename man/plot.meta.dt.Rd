\name{plot.meta.dt}
\alias{plot.meta.dt}
\title{
Plot for Meta-Analysis of Diagnostic Tests
}
\description{
Visualizes meta-analysis of diagnostic tests by presenting summary results, such as ROC (receiver operating characteristic) curve, overall sensitivity and overall specificity (1 \eqn{-} specificity), and their confidence and prediction regions.
}
\usage{
\method{plot}{meta.dt}(x, add = FALSE, xlab, ylab, alpha,
     studies = TRUE, cex.studies, col.studies, pch.studies,
     roc, col.roc, lty.roc, lwd.roc, weight = FALSE,
     eqline, col.eqline, lty.eqline, lwd.eqline,
     overall = TRUE, cex.overall, col.overall, pch.overall,
     confid = TRUE, col.confid, lty.confid, lwd.confid,
     predict = FALSE, col.predict, lty.predict, lwd.predict, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"meta.dt"} created by the function \code{\link{meta.dt}}.
}
  \item{add}{
  a logical value indicating if the plot is added to an already existing plot.
}
  \item{xlab}{
  a label for the x axis; the default is "1 - Specificity".
}
  \item{ylab}{
  a label for the y axis; the default is "Sensitivity".
}
  \item{alpha}{
  a numeric value specifying the statistical significance level for the confidence and prediction regions. If not specified, the plot uses the significance level stored in \code{x} (i.e., \code{x$alpha}).
}
  \item{studies}{
  a logical value indicating if the individual studies are presented in the plot.
}
  \item{cex.studies}{
  the size of points representing individual studies (the default is 1).
}
  \item{col.studies}{
  the color of points representing individual studies (the default is \code{"black"}).
}
  \item{pch.studies}{
  the symbol of points representing individual studies (the default is 1, i.e., circle).
}
  \item{roc}{
  a logical value indicating if the ROC curve is presented in the plot. The default is \code{TRUE} for the summary ROC approach (\code{x$method} = \code{"s.roc"}) and is \code{FALSE} for the bivariate (generalized) linear mixed model (\code{x$method} = \code{"biv.lmm"} or \code{"biv.glmm"}).
}
  \item{col.roc}{
  the color of the ROC curve (the default is \code{"black"}).
}
  \item{lty.roc}{
  the line type of the ROC curve (the default is 1, i.e., solid line).
}
  \item{lwd.roc}{
  the line width of the ROC curve (the default is 1).
}
  \item{weight}{
  a logical value indicating if the weighted (\code{TRUE}) or unweighted (\code{FALSE}, the default) regression is used for the summary ROC approach (when \code{x$method} is \code{"s.roc"}).
}
  \item{eqline}{
  a logical value indicating if the line of sensitivity equaling to specificity is presented in the plot.
}
  \item{col.eqline}{
  the color of the equality line (the default is \code{"black"}).
}
  \item{lty.eqline}{
  the type of the equality line (the default is 4, i.e., dot-dash line).
}
  \item{lwd.eqline}{
  the width of the equality line (the default is 1).
}
  \item{overall}{
  a logical value indicating if the overall sensitivity and overall specificity are presented in the plot. This and the following arguments are used for the bivariate (generalized) linear mixed model (\code{x$method} = \code{"biv.lmm"} or \code{"biv.glmm"}).
}
  \item{cex.overall}{
    the size of the point representing the overall sensitivity and overall specificity  (the default is 1).
}
  \item{col.overall}{
    the color of the point representing the overall sensitivity and overall specificity  (the default is \code{"black"}).
}
  \item{pch.overall}{
    the symbol of the point representing the overall sensitivity and overall specificity  (the default is 15, i.e., filled square).
}
  \item{confid}{
  a logical value indicating if the confidence region of the overall sensitivity and overall specificity is presented in the plot.
}
  \item{col.confid}{
  the line color of the confidence region (the default is \code{"black"}).
}
  \item{lty.confid}{
  the line type of the confidence region (the default is 2, i.e., dashed line).
}
  \item{lwd.confid}{
  the line width of the confidence region (the default is 1).
}
  \item{predict}{
  a logical value indicating if the prediction region of the overall sensitivity and overall specificity is presented in the plot.
}
  \item{col.predict}{
  the line color of the prediction region (the default is \code{"black"}).
}
  \item{lty.predict}{
  the line type of the prediction region (the default is 3, i.e., dotted line).
}
  \item{lwd.predict}{
  the line width of the prediction region (the default is 1).
}
  \item{\dots}{
  other arguments that can be passed to the function \code{\link[graphics]{plot.default}}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
None.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{meta.dt}}, \code{\link{print.meta.dt}}
}
%%\examples{
%%}
\keyword{diagnostic test}
\keyword{plot}
