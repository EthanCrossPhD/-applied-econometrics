####################################################
# Chapter 8 — Autocorrelation (BASIC)
####################################################

run_ch8_autocorrelation <- function(n = 200, rho = 0.7) {

  set.seed(321)

  x <- 1:n

  u <- numeric(n)
  u[1] <- rnorm(1)

  for (t in 2:n) {
    u[t] <- rho * u[t - 1] + rnorm(1)
  }

  y <- 2 + 0.05 * x + u

  model <- lm(y ~ x)

  interpretation <- paste0(
    "Autocorrelation occurs when the error term is correlated over time.\n\n",
    "Positive autocorrelation produces smooth residual patterns and ",
    "leads to underestimated standard errors."
  )

  list(
    data = data.frame(t = x, y = y, u = u),
    model = model,
    interp = interpretation
  )
}