####################################################
# Chapter 7 — Heteroskedasticity (BASIC)
####################################################

run_ch7_heteroskedasticity <- function(n = 300) {

  set.seed(123)

  x <- runif(n, 0, 10)

  # Heteroskedastic error
  u <- rnorm(n, mean = 0, sd = 0.5 * x)

  y <- 1 + 0.8 * x + u

  model <- lm(y ~ x)

  interpretation <- paste0(
    "Heteroskedasticity occurs when the variance of the error term ",
    "depends on the level of an explanatory variable.\n\n",
    "OLS coefficients remain unbiased, but standard errors ",
    "are no longer reliable."
  )

  list(
    data = data.frame(y = y, x = x),
    model = model,
    interp = interpretation
  )
}