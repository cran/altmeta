\name{metapb}
\alias{metapb}

\title{
Detect and Quantify Publication Bias
}
\description{
Performs the regression test and calculates skewness for detecting and quantifying publication bias.
}
\usage{
metapb(y, s2, model)
}

\arguments{
  \item{y}{
  a numeric vector indicating the observed effect sizes in the collected studies; they are assumed to be normally distributed.
}
  \item{s2}{
  a numeric vector indicating the within-study variances.
}
  \item{model}{
  a characher string specifying the fixed-effect (\code{"FE"}) or random-effects (\code{"RE"}) model. If not specified, this function uses the \eqn{Q} statistic to test for heterogeneity: if the \eqn{p}-value is smaller than 0.05, \code{model} is set to \code{"RE"}; otherwise, \code{model = "FE"}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function returns a list containing measures of publication bias, their 95\% confidence intervals, and \eqn{p}-values.
}
\references{
Egger M, Smith GD, Schneider M, and Minder C (1997). "Bias in meta-analysis detected by a simple, graphical test." \emph{BMJ}, \bold{315}(7109), 629--634.
}

%%\note{
%%  ~~further notes~~
%%}

\examples{
data("slf")
attach(slf)
metapb(y, s2)
detach(slf)

data("ha")
attach(ha)
metapb(y, s2)
detach(ha)

data("lcj")
attach(lcj)
metapb(y, s2)
detach(lcj)
}