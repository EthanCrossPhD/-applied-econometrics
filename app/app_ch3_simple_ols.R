library(tidyverse)

run_simple_ols <- function() {
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  y <- 1 + 2 * x + rnorm(n)

  df <- tibble(x = x, y = y)
  model <- lm(y ~ x, data = df)

  coefs <- summary(model)$coefficients

  table_out <- tibble(
    Term = rownames(coefs),
    Estimate = round(coefs[, 'Estimate'], 3),
    `Std. Error` = round(coefs[, 'Std. Error'], 3),
    `p-value` = round(coefs[, 'Pr(>|t|)'], 4)
  )

  list(
    data = df,
    model = model,
    table_out = table_out
  )
}
