pb.timelag <- function(y, v, time, B = 5000, tau2 = NULL) {

  if (!is.numeric(y) || !is.numeric(v) || !is.numeric(time)) {
    stop("y, v, and time must be numeric vectors.")
  }

  if (length(y) != length(v) || length(y) != length(time)) {
    stop("y, v, and time must have the same length.")
  }

  if (length(y) < 3L || anyNA(y) || anyNA(v) || anyNA(time)) {
    stop("At least three studies with complete data are required.")
  }

  if (any(v <= 0)) {
    stop("All within-study variances must be positive.")
  }

  if (length(B) != 1L || !is.finite(B) || B < 2L || B != as.integer(B)) {
    stop("B must be an integer greater than or equal to 2.")
  }

  if (!is.null(tau2) &&
      (length(tau2) != 1L || !is.finite(tau2) || tau2 < 0)) {
    stop("tau2 must be NULL or a non-negative finite number.")
  }

  ord <- order(time)
  y <- y[ord]
  v <- v[ord]

  if (is.null(tau2)) {
    tau2 <- metafor::rma.uni(yi = y, vi = v, method = "REML")$tau2
  }

  get.statistic <- function(y, v, tau2) {
    K <- length(y)
    S <- numeric(K - 1L)

    for (i in seq_len(K - 1L)) {
      S[i] <- sum(
        (y[(i + 1L):K] - y[i]) /
          sqrt(v[(i + 1L):K] + v[i] + 2 * tau2)
      )
    }

    scaled.S <- abs(S) / sqrt((K - 1L):1L)

    c(
      vapply(1:8, function(gamma) sum(scaled.S^gamma), numeric(1)),
      max(scaled.S)
    )
  }

  T.obs <- get.statistic(y, v, tau2)
  T.perm <- matrix(NA_real_, nrow = B, ncol = 9L)

  for (b in seq_len(B)) {
    index <- sample.int(length(y))
    T.perm[b, ] <- get.statistic(y[index], v[index], tau2)
  }

  p.fixed <-
    (colSums(sweep(T.perm, 2L, T.obs, FUN = ">=")) + 1) / (B + 1)

  p.perm <- matrix(NA_real_, nrow = B, ncol = 9L)

  for (b in seq_len(B)) {
    p.perm[b, ] <-
      (colSums(
        sweep(
          T.perm[-b, , drop = FALSE],
          2L,
          T.perm[b, ],
          FUN = ">="
        )
      ) + 1) / B
  }

  hybrid.obs <- min(p.fixed)
  hybrid.perm <- apply(p.perm, 1L, min)
  p.hybrid <- (sum(hybrid.perm <= hybrid.obs) + 1) / (B + 1)

  test.names <- c(paste0("T", 1:8), "TInf")

  list(
    p.value = stats::setNames(
      c(p.fixed, p.hybrid),
      c(test.names, "Hybrid")
    ),
    statistic = stats::setNames(T.obs, test.names),
    tau2 = tau2,
    B = B
  )
}
