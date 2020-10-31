\name{meta.dt}
\alias{meta.dt}
\title{
Meta-Analysis of Diagnostic Tests
}
\description{
Performs a meta-analysis of diagnostic tests using approaches described in Reitsma et al.\sspace(2005) and Chu and Cole (2006).
}
\usage{
meta.dt(tp, fp, fn, tn, data, method = "biv.glmm", alpha = 0.05, ...)
}
\arguments{
  \item{tp}{
  counts of true positives.
}
  \item{fp}{
  counts of false positives.
}
  \item{fn}{
  counts of false negatives.
}
  \item{tn}{
  counts of true negatives.
}
  \item{data}{
  an optional data frame containing the meta-analysis dataset. If \code{data} is specified, the previous arguments, \code{tp}, \code{fp}, \code{fn}, and \code{tn}, should be specified as their corresponding column names in \code{data}.
}
  \item{method}{
  a character string specifying the method used to implement the meta-analysis of diagnostic tests. It should be one of \code{"s.roc"} (summary ROC approach), \code{"biv.lmm"} (bivariate linear mixed model), and \code{"biv.glmm"} (bivariate generalized linear mixed model, the default). See details.
}
  \item{alpha}{
  a numeric value specifying the statistical significance level.
}
  \item{\dots}{
  other arguments that can be passed to the function \code{\link[stats]{lm}} (when \code{method} = \code{"s.roc"}), the function \code{\link[metafor]{rma.mv}} in the package \pkg{metafor} (when \code{method} = \code{"biv.lmm"}), or the function \code{\link[lme4]{glmer}} in the package \pkg{lme4} (when \code{method} = \code{"biv.glmm"}).
}
}
\details{
Suppose a meta-analysis of diagnostic tests contains \eqn{N} studies. Each study reports the counts of true positives, false positives, false negatives, and true negatives, denoted by \eqn{TP_i}, \eqn{FP_i}, \eqn{FN_i}, and \eqn{TN_i}, respectively. The study-specific estimates of sensitivity and specificity are calculated as \eqn{Se_i = TP_i/(TP_i + FN_i)} and \eqn{Sp_i = TN_i/(FP_i + TN_i)} for \eqn{i = 1, \cdots, N}. They are analyzed on the logarithmic scale in the meta-analysis. When using the summary ROC (receiver operating characteristic) approach or the bivariate linear mixed model, 0.5 needs to be added to all four counts in a study when at least one count is zero.

The summary ROC approach first calculates
\deqn{D_i = \log \left( \frac{Se_i}{1 - Se_i} \right) + \log \left( \frac{Sp_i}{1 - Sp_i} \right);}
\deqn{S_i = \log \left( \frac{Se_i}{1 - Se_i} \right) - \log \left( \frac{Sp_i}{1 - Sp_i} \right),}
where \eqn{D_i} represents the log diagnostic odds ratio (DOR) in study \eqn{i}. A linear regression is then fitted:
\deqn{D_i = \alpha + \beta \cdot S_i.}
The regression could be either unweighted or weighted; this function performs both versions. If weighted, the study-specific weights are the inverse of the variances of \eqn{D_i}, i.e., \eqn{1/TP_i + 1/FP_i + 1/FN_i + 1/TN_i}. Based on the estimated regression intercept \eqn{\hat{\alpha}} and slope \eqn{\hat{\beta}}, one may obtain the DOR at mean of \eqn{S_i}, Q point, summary ROC curve, and area under the curve (AUC). The Q point is the point on the summary ROC curve where sensitivity and specificity are equal. The ROC curve is given by
\deqn{Se = \left\{ 1 + e^{-\hat{\alpha}/(1 - \hat{\beta})} \cdot [Sp/(1 - Sp)]^{(1 + \hat{\beta})/(1 - \hat{\beta})} \right\}^{-1}.}
See more details of the summary ROC approach in Moses et al.\sspace(1993) and Irwig et al.\sspace(1995).

The bivariate linear mixed model described in Reitsma et al.\sspace(2005) assumes that the logit sensitivity and logit specificity independently follow normal distributions within each study: \eqn{g(Se_i) \sim N(\theta_{i, Se}, s_{i, Se}^2)} and \eqn{g(Sp_i) \sim N(\theta_{i, Sp}, s_{i, Sp}^2)}, where \eqn{g(\cdot)} denotes the logit function. The within-study variances are calculated as \eqn{s_{i, Se}^2 = 1/TP_i + 1/FN_i} and \eqn{s_{i, Sp}^2 = 1/FP_i + 1/TN_i}. The parameters \eqn{\theta_{i, Se}} and \eqn{\theta_{i, Sp}} are the underlying true sensitivity and specificity (on the logit scale) in study \eqn{i}. They are assumed to be random effects, jointly following a bivariate normal distribution:
\deqn{\left(\theta_{i, Se}, \theta_{i, Sp}\right)^\prime \sim N\left((\mu_{Se}, \mu_{Sp})^\prime, \mathbf{\Sigma} \right),}
where \eqn{\mathbf{\Sigma}} is the between-study variance-covariance matrix. The diagonal elements of \eqn{\mathbf{\Sigma}} are \eqn{\sigma_{Se}^2} and \eqn{\sigma_{Sp}^2}, representing the heterogeneity variances of sensitivities and specificities (on the logit scale), respectively. The correlation coefficient is \eqn{\rho}.

The bivariate generalized linear mixed model described in Chu and Cole (2006) refines the bivariate linear mixed model by directly modeling the counts of true positives and true negatives. This approach does not require the assumption that the logit sensitivity and logit specificity approximately follow normal distributions within studies, which could be seriously violated in the presence of small data counts. It also avoids corrections for zero counts. Specificially, the counts of true positives and true negatives are modeled using binomial likelihoods:
\deqn{TP_i \sim Bin(TP_i + FN_i, Se_i);}
\deqn{TN_i \sim Bin(FP_i + TN_i, Sp_i);}
\deqn{\left( g(Se_i), g(Sp_i) \right)^\prime \sim N\left((\mu_{Se}, \mu_{Sp})^\prime, \mathbf{\Sigma} \right).}
See more details in Chu and Cole (2006) and Ma et al.\sspace(2016).

For both the bivariate linear mixed model and bivariate generalized linear mixed model, \eqn{\mu_{Se}} and \eqn{\mu_{Sp}} represent the overall sensitivity and specificity (on the logit scale) across studies, respectively, and \eqn{\mu_{Se} + \mu_{Sp}} represents the log DOR. The summary ROC curve may be constructed as
\deqn{Se = \left\{ 1 + e^{-\hat{\mu}_{Se} + \hat{\mu}_{Sp} \cdot \hat{\rho} \hat{\sigma}_{Se}/\hat{\sigma}_{Sp}} \cdot [Sp/(1 - Sp)]^{-\rho \hat{\sigma}_{Se}/\hat{\sigma}_{Sp}} \right\}^{-1}.}
}
\value{
This function returns a list of the meta-analysis results. When \code{method} = \code{"s.roc"}, the list consists of the regression intercept (\code{inter.unwtd}), slope (\code{slope.unwtd}), their variance-covariance matrix (\code{vcov.unwtd}), DOR at mean of \eqn{S_i} (\code{DOR.meanS.unwtd}) with its confidence interval (\code{DOR.meanS.unwtd.ci}), Q point (\code{Q.unwtd}) with its confidence interval (\code{Q.unwtd.ci}), and AUC (\code{AUC.unwtd}) for the unweighted regression; it also consists of the counterparts for the weighted regression. When \code{method} = \code{"biv.lmm"} or \code{"biv.glmm"}, the list consists of the overall sensitivity (\code{sens.overall}) with its confidence interval (\code{sens.overall.ci}), overall specificity (\code{spec.overall}) with its confidence interval (\code{spec.overall.ci}), overall DOR (\code{DOR.overall}) with its confidence interval (\code{DOR.overall.ci}), AUC (\code{AUC}), estimated \eqn{\mu_{Se}} (\code{mu.sens}), \eqn{\mu_{Sp}} (\code{mu.spec}), their variance-covariance matrix (\code{mu.vcov}), estimated \eqn{\sigma_{Se}} (\code{sig.sens}), \eqn{\sigma_{Sp}} (\code{sig.spec}), and \eqn{\rho} (\code{rho}). In addition, the list includes the method used to perform the meta-analysis of diagnostic tests (\code{method}), significance level (\code{alpha}), and original data (\code{data}).
}
\references{
Chu H, Cole SR (2006). "Bivariate meta-analysis of sensitivity and specificity with sparse data: a generalized linear mixed model approach." \emph{Journal of Clinical Epidemiology}, \bold{59}(12), 1331--1332. <\doi{10.1016/j.jclinepi.2006.06.011}>

Irwig L, Macaskill P, Glasziou P, Fahey M (1995). "Meta-analytic methods for diagnostic test accuracy." \emph{Journal of Clinical Epidemiology}, \bold{48}(1), 119--130. <\doi{10.1016/0895-4356(94)00099-C}>

Ma X, Nie L, Cole SR, Chu H (2016). "Statistical methods for multivariate meta-analysis of diagnostic tests: an overview and tutorial." \emph{Statistical Methods in Medical Research}, \bold{25}(4), 1596--1619. <\doi{10.1177/0962280213492588}>

Moses LE, Shapiro D, Littenberg B (1993). "Combining independent studies of a diagnostic test into a summary ROC curve: data-analytic approaches and some additional considerations." \emph{Statistics in Medicine}, \bold{12}(14), 1293--1316. <\doi{10.1002/sim.4780121403}>

Reitsma JB, Glas AS, Rutjes AWS, Scholten RJPM, Bossuyt PM, Zwinderman AH (2005). "Bivariate analysis of sensitivity and specificity produces informative summary measures in diagnostic reviews." \emph{Journal of Clinical Epidemiology}, \bold{58}(10), 982--990. <\doi{10.1016/j.jclinepi.2005.02.022}>

Zhang H, Lu N, Feng C, Thurston SW, Xia Y, Zhu L, Tu XM (2011). "On fitting generalized linear mixed-effects models for binary responses using different statistical packages." \emph{Statistics in Medicine}, \bold{30}(20), 2562--2572. <\doi{10.1002/sim.4265}>
}
%%\author{
%%  ~~who you are~~
%%}
\note{
The original articles by Reitsma et al.\sspace(2005) and Chu and Cole (2006) used SAS to implement (generalized) linear mixed models (specifically, \code{PROC MIXED} and \code{PROC NLMIXED}); this function imports \code{\link[metafor]{rma.mv}} from the package \pkg{metafor} and \code{\link[lme4]{glmer}} from the package \pkg{lme4} for implementing these models. The estimation approaches adopted in SAS and the R packages \pkg{metafor} and \pkg{lme4} may differ, which may impact the results. See, for example, Zhang et al.\sspace(2011).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maprop.twostep}}, \code{\link{meta.biv}}, \code{\link{plot.meta.dt}}, \code{\link{print.meta.dt}}
}
\examples{
data("dat.scheidler")
out1 <- meta.dt(tp, fp, fn, tn, data = dat.scheidler[dat.scheidler$dt == "MRI",],
  method = "s.roc")
out1
plot(out1)
out2 <- meta.dt(tp, fp, fn, tn, data = dat.scheidler[dat.scheidler$dt == "MRI",],
  method = "biv.lmm")
out2
plot(out2, predict = TRUE)
out3 <- meta.dt(tp, fp, fn, tn, data = dat.scheidler[dat.scheidler$dt == "MRI",],
  method = "biv.glmm")
out3
plot(out3, add = TRUE, studies = FALSE,
  col.roc = "blue", col.overall = "blue", col.confid = "blue",
  predict = TRUE,col.predict = "blue")

data("dat.smith")
out4 <- meta.dt(tp, fp, fn, tn, data = dat.smith, method = "biv.glmm")
out4
plot(out4, predict = TRUE)
}
\keyword{diagnostic test}
\keyword{generalized linear mixed model}