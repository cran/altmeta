\name{metahet}
\alias{metahet}

\title{
Meta-Analysis Heterogeneity Measures
}
\description{
Calculates various between-study heterogeneity measures in meta-analysis, including the conventional measures (e.g., \eqn{I^2}) and the alternative measures (e.g., \eqn{I_r^2}) which are robust to outlying studies; p-values of various tests are also calculated.
}
\usage{
metahet(y, s2, n.resam = 1000)
}

\arguments{
  \item{y}{
  a numeric vector indicating the observed effect sizes in the collected studies; they are assumed to be normally distributed.
}
  \item{s2}{
  a numeric vector indicating the within-study variances.
}
  \item{n.resam}{
  a positive integer indicating the number of resampling iterations for calculating p-values of test statistics and 95\% confidence interval of heterogeneity measures.
}
}
\details{
Suppose that a meta-analysis collects \eqn{n} studies. The observed effect size in study \eqn{i} is \eqn{y_i} and its within-study variance is \eqn{s^{2}_{i}}. Also, the inverse-variance weight is \eqn{w_i = 1 / s^{2}_{i}}. The fixed-effect estimate of overall effect size is \eqn{\bar{\mu} = \sum_{i = 1}^{n} w_i y_i / \sum_{i = 1}^{n} w_i}. The conventional test statistic for heterogeneity is
\deqn{Q = \sum_{i = 1}^{n} w_i (y_{i} - \bar{\mu})^2.}
Based on the \eqn{Q} statistic, the method-of-moments estimate of the between-study variance \eqn{\hat{\tau}_{DL}^2} is (DerSimonian and Laird, 1986)
\deqn{\hat{\tau}^2_{DL} = \max \left\{ 0, \frac{Q - (n - 1)}{\sum_{i = 1}^{n} w_{i} - \sum_{i = 1}^{n} w_{i}^{2} / \sum_{i = 1}^{n} w_{i}} \right\}.}
Also, the \eqn{H} and \eqn{I^2} statistics (Higgins and Thompson, 2002; Higgins et al., 2003) are widely used in practice because they do not depend on the number of collected studies \eqn{n} and the effect size scale; these two statistics are defined as
\deqn{H = \sqrt{Q/(n - 1)};}
\deqn{I^{2} = \frac{Q - (n - 1)}{Q}.}
Specifically, the \eqn{H} statistic reflects the ratio of the standard deviation of the underlying mean from a random-effects meta-analysis compared to the standard deviation from a fixed-effect meta-analysis; the \eqn{I^2} statistic describes the proportion of total variance across studies that is due to heterogeneity rather than sampling error.

Outliers are frequently present in meta-analysis, and they may have great impact on the above heterogeneity measures. Alternatively, to be more robust to outliers, the test statistic may be modified as (Lin et al., 2016+):
\deqn{Q_{r} = \sum_{i = 1}^{n} \sqrt{w_i} |y_{i} - \bar{\mu}|.}
Based on the \eqn{Q_r} statistic, the method-of-moments estimate of between-study variance \eqn{\hat{\tau}_r^2} is defined as the solution to
\deqn{Q_r \sqrt{\frac{\pi}{2}} = \sum_{i = 1}^{n} \left\{1 - \frac{w_{i}}{\sum_{j = 1}^{n} w_{j}} + \tau^{2} \left[ w_{i} - \frac{2 w_{i}^{2}}{\sum_{j = 1}^{n} w_{j}} + \frac{w_{i} \sum_{j = 1}^{n} w_{j}^{2}}{(\sum_{j = 1}^{n} w_{j})^2} \right]\right\}.}
If no positive solution exists to the equation above, set \eqn{\hat{\tau}_{r}^{2} = 0}. The counterparts of the \eqn{H} and \eqn{I^2} statistics are defined as
\deqn{H_{r} = Q_r \sqrt{\pi/[2 n (n - 1)]};}
\deqn{I_{r}^{2} = \frac{Q_{r}^{2} - 2 n (n - 1) / \pi}{Q_{r}^{2}}.}

To further improve the robustness of heterogeneity assessment, the weighted \emph{mean} in the \eqn{Q_r} statistic may be replaced by the weighted \emph{median} \eqn{\hat{\mu}_m}, which is the solution to \eqn{\sum_{i = 1}^{n} w_i [I (\theta \geq y_i) - 0.5] = 0} with respect to \eqn{\theta}. The new test statistic is
\deqn{Q_m = \sum_{i = 1}^{n} \sqrt{w_i} |y_{i} - \hat{\mu}_m|.}
Based on \eqn{Q_m}, the new estimator of between-study variance \eqn{\hat{\tau}_m^2} is the solution to
\deqn{Q_m \sqrt{\pi/2} = \sum_{i = 1}^{n} \sqrt{(s_i^2 + \tau^2)/s_i^2}.}
The counterparts of the \eqn{H} and \eqn{I^2} statistics are
\deqn{H_m = \frac{Q_m}{n} \sqrt{\pi/2};}
\deqn{I_m^2 = \frac{Q_m^2 - 2 n^2/\pi}{Q_m^2}.}
}
\value{
This function returns a list containing p-values of various heterogeneity tests and various heterogeneity measures with 95\% confidence intervals.
}
\references{
DerSimonian R and Laird N (1986). "Meta-analysis in clinical trials." \emph{Controlled Clinical Trials}, \bold{7}(3), 177--188.

Higgins JPT and Thompson SG (2002). "Quantifying heterogeneity in a meta-analysis." \emph{Statistics in Medicine}, \bold{21}(11), 1539--1558.

Higgins JPT, Thompson SG, Deeks JJ, and Altman DG (2003). "Measuring inconsistency in meta-analyses." \emph{BMJ}, \bold{327}(7414), 557--560.

Lin L, Chu H, and Hodges JS (2016+). "Alternative measures of between-study heterogeneity in meta-analysis: reducing the impact of outlying studies".
}


\examples{
data("aex")
set.seed(1234)
attach(aex)
metahet(y, s2, 100)
#metahet(y, s2, 1000)
detach(aex)

data("hipfrac")
set.seed(1234)
attach(hipfrac)
metahet(y, s2, 100)
#metahet(y, s2, 1000)
detach(hipfrac)
}