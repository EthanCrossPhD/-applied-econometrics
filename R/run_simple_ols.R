
run_simple_ols <- function(fig_path = "figures/fig_ols_fit.png") {

  library(tibble)
  library(ggplot2)
  library(broom)
  library(dplyr)

  dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)

  set.seed(123)
  n <- 100
  ols_data <- tibble(
    x = rnorm(n),
    y = 1 + 2 * x + rnorm(n)
  )

  model <- lm(y ~ x, data = ols_data)

  ols_data <- ols_data |>
    mutate(y_hat = predict(model))

  p <- ggplot(ols_data, aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.6) +
    geom_line(aes(y = y_hat), color = "blue", linewidth = 1) +
    labs(
      x = "Explanatory variable (x)",
      y = "Outcome variable (y)",
      title = "OLS fitted line"
    ) +
    theme_minimal()

  ggsave(
    filename = fig_path,
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )

  table_out <- broom::tidy(model) |>
    mutate(
      estimate  = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 2)
    )

  list(
    model   = model,
    figpath = fig_path,
    table   = table_out
  )
}

