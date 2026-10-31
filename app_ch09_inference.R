# --------------------------------------------------
# Inference App: Classical vs Robust vs Clustered SE
# --------------------------------------------------

run_ch09_inference <- function(
  N = 500,
  G = 50,
  beta = 1,
  rho = 0.6,
  seed = 123
) {
  set.seed(seed)

  # Assign groups
  group <- rep(1:G, each = N / G)

  # Regressor
  x <- rnorm(N)

  # Group-level shock (induces correlation)
  u_g <- rnorm(G)
  u <- rho * u_g[group] + rnorm(N)

  # Outcome
  y <- beta * x + u

  data <- data.frame(
    y = y,
    x = x,
    group = group
  )

  # OLS
  ols <- lm(y ~ x, data = data)

  # Classical SE
  se_classical <- summary(ols)$coefficients["x", "Std. Error"]

  # Robust SE (White)
  X <- model.matrix(ols)
  XtX_inv <- solve(crossprod(X))
  uhat <- resid(ols)
  meat_white <- crossprod(X * uhat)
  vcov_white <- XtX_inv %*% meat_white %*% XtX_inv
  se_robust <- sqrt(vcov_white["x", "x"])

  # Clustered SE
  clusters <- unique(group)
  meat_cluster <- matrix(0, ncol(X), ncol(X))
  for (g in clusters) {
    idx <- group == g
    Xg <- X[idx, , drop = FALSE]
    ug <- uhat[idx]
    meat_cluster <- meat_cluster + crossprod(Xg, ug) %*% t(crossprod(Xg, ug))
  }

  vcov_cluster <- XtX_inv %*% meat_cluster %*% XtX_inv
  se_cluster <- sqrt(vcov_cluster["x", "x"])

  list(
    data = data,
    beta_true = beta,
    beta_hat = coef(ols)["x"],
    se = data.frame(
      Classical = se_classical,
      Robust = se_robust,
      Clustered = se_cluster
    )
  )
}
