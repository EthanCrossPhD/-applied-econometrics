####################################################
# Chapter 10 — Panel Data (Intro, BASIC)
####################################################

run_ch10_panel <- function(N = 30, T = 8) {

  set.seed(999)

  id <- rep(1:N, each = T)
  time <- rep(1:T, times = N)

  # Individual fixed effects
  alpha <- rep(rnorm(N, mean = 0, sd = 2), each = T)

  x <- rnorm(N * T)
  u <- rnorm(N * T)

  y <- 1 + 0.6 * x + alpha + u

  df <- data.frame(
    id = id,
    time = time,
    y = y,
    x = x,
    alpha = alpha
  )

  # Pooled OLS (ignores panel structure)
  pooled <- lm(y ~ x, data = df)

  # Fixed Effects (within transformation)
  fe <- lm(y ~ x + factor(id), data = df)

  interpretation <- paste0(
    "Panel data combine variation across individuals and over time.\n\n",
    "Pooled OLS ignores unobserved individual heterogeneity.\n\n",
    "Fixed effects control for time-invariant individual characteristics."
  )

  list(
    data = df,
    pooled = pooled,
    fe = fe,
    interp = interpretation
  )
}