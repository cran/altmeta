#' Alternative tests and measures for between-study inconsistency in meta-analysis
#'
#' This function jointly computes the expected gamma statistics
#' and the bootstrap-based adaptive sum of powered score (ASPU) p-values
#' for assessing between-study heterogeneity in meta-analysis.
#'
#' @param y Numeric vector of observed effect estimates, or the name of a column in \code{data}.
#' @param s2 Numeric vector of within-study variances corresponding to \code{y}, or the name of a column in \code{data}.
#' @param data Optional data frame containing the variables \code{y} and \code{s2}.
#' @param gam Numeric vector of power parameters \eqn{\gamma}, e.g., \code{1:6}.
#' @param testinf Logical; if \code{TRUE}, includes the \eqn{Q_{\infty}} statistic (maximum standardized deviation).
#' @param iter.resam Integer; number of bootstrap resamples used to estimate expected values and p-values.
#'
#' @details
#' The function generalizes the adaptive sum of powered score (ASPU) framework
#' by combining standardized powered deviations across studies.
#' Specifically, for each power parameter gamma, it calculates a statistic
#' that measures the average standardized deviation of individual study effects
#' from the overall mean, with greater gamma values placing more weight on
#' extreme deviations.
#'
#' For each gamma, the expected value of this statistic is obtained through
#' parametric bootstrap under the null hypothesis of homogeneity. The observed
#' statistic is then standardized by subtracting its expected value and dividing
#' by the observed total deviation, resulting in the expected gamma (E-gamma)
#' measure.
#'
#' In addition, a hybrid measure is computed by combining the p-values across
#' all gamma values using the minimum-p combination rule. This hybrid measure
#' provides an overall summary of between-study inconsistency that integrates
#' evidence from multiple power levels.
#'
#' @return A list named \code{out} containing:
#' \itemize{
#'   \item \code{E_gamma}: Vector of E-gamma statistics for each \eqn{\gamma}, including \eqn{E_{\infty}} and the hybrid \eqn{E_{Lhyb}} term.
#'   \item \code{p_gamma}: Bootstrap p-values corresponding to each \eqn{\gamma}.
#'   \item \code{p_hyb}: Hybrid p-value combining all \eqn{\gamma} values.
#'   \item \code{gam}: Vector of power parameters used in the test.
#'   \item \code{table}: Combined summary table for reporting.
#' }
#'
#' @examples
#' data("dat.adams")
#' set.seed(123)
#' metahet.hybrid(y, s2, data = dat.adams, gam = 1:8,
#'                       testinf = TRUE, iter.resam = 200)
#'
#' @keywords heterogeneity
#' @importFrom stats rnorm
#' @export
metahet.hybrid <- function(y, s2, data, gam, testinf = TRUE, iter.resam = 200) {
  # --- input checks ---
  if (missing(y)) stop("Please specify the effect size variable 'y'.")
  if (missing(s2)) stop("Please specify the within-study variance variable 's2'.")
  if (!missing(data)) {
    y  <- eval(substitute(y),  data, parent.frame())
    s2 <- eval(substitute(s2), data, parent.frame())
  }
  if (length(y) != length(s2) || any(s2 <= 0))
    stop("Input data error: 'y' and 's2' must have equal length and s2 > 0.")
  
  # --- compute weighted mean under homogeneity ---
  n <- length(y)
  mu_ce <- sum(y / s2) / sum(1 / s2)
  
  # --- compute Q_gamma statistics ---
  N_gam <- length(gam)
  Q <- sapply(gam, function(g) sum((sqrt(1 / s2) * abs(y - mu_ce))^g))
  if (testinf) {
    Q <- c(Q, max(sqrt(1 / s2) * abs(y - mu_ce)))
    N_gam <- N_gam + 1
  }
  
  # --- parametric bootstrap ---
  Q.resam <- matrix(NA, iter.resam, N_gam)
  for (b in 1:iter.resam) {
    s2.temp <- sample(s2, n, replace = TRUE)
    y.temp  <- rnorm(n, mean = mu_ce, sd = sqrt(s2.temp))
    mu.temp <- sum(y.temp / s2.temp) / sum(1 / s2.temp)
    Q.temp  <- sapply(gam, function(g) sum((sqrt(1 / s2.temp) * abs(y.temp - mu.temp))^g))
    if (testinf) Q.temp <- c(Q.temp, max(sqrt(1 / s2.temp) * abs(y.temp - mu.temp)))
    Q.resam[b, ] <- Q.temp
  }
  
  # --- E_gamma ---
  EQ <- colMeans(Q.resam, na.rm = TRUE)
  E_gamma <- pmax(0, Q - EQ) / Q
  
  # --- empirical p-values ---
  p_gamma <- sapply(1:N_gam, function(i)
    (sum(Q.resam[, i] >= Q[i], na.rm = TRUE) + 1) / (iter.resam + 1))
  p_gammab <- matrix(NA, iter.resam, N_gam)
  for (b in 1:iter.resam) {
    for (i in 1:N_gam) {
      p_gammab[b, i] <- (sum(Q.resam[-b, i] >= Q.resam[b, i], na.rm = TRUE) + 1) / iter.resam
    }
  }
  Qhyb <- suppressWarnings(min(p_gamma, na.rm = TRUE))
  Qhyb_b <- apply(p_gammab, 1, min, na.rm = TRUE)
  p_hyb <- (sum(Qhyb_b <= Qhyb, na.rm = TRUE) + 1) / (iter.resam + 1)
  
  # --- E_hyb (hybrid E_gamma) ---
  Lhyb <- -log10(Qhyb)
  Lhyb_b <- -log10(Qhyb_b)
  E_Lhyb <- mean(Lhyb_b, na.rm = TRUE)
  E_hyb <- max(0, Lhyb - E_Lhyb) / Lhyb
  
  # --- label construction ---
  labels <- c(as.character(gam), if (testinf) "Inf", "Hybrid")
  Evals  <- c(E_gamma, E_hyb)
  Pvals  <- c(p_gamma, p_hyb)
  
  # --- summary table ---
  table_out <- data.frame(
    gamma   = labels,
    E_gamma = round(Evals, 4),
    p_gamma = round(Pvals, 4),
    stringsAsFactors = FALSE
  )
  
  # --- output ---
  out <- list(
    E_gamma = E_gamma,
    E_hyb   = E_hyb,
    p_gamma = p_gamma,
    p_hyb   = p_hyb,
    gam     = gam,
    table   = table_out
  )
  class(out) <- "metahet.hybrid"
  return(out)
}

#' @export
print.metahet.hybrid <- function(x, digits = 4, ...) {
  cat("\nAlternative tests results and measures for heterogeneity\n")
  if (!is.null(x$table) && nrow(x$table) > 0) {
    print(x$table, row.names = FALSE, right = TRUE)
  } else {
    cat("No valid results.\n")
  }
  invisible(x)
}
