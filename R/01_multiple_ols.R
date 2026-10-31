
# =========================================================
# Chapter: Multiple OLS
# Goal: Estimating ceteris paribus effects
# =========================================================

source("R/00_global_setup.R")

set.seed(123)

n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
u  <- rnorm(n, sd = 1)

y <- 1 + 2*x1 - 1.5*x2 + u

df <- tibble(y, x1, x2)

m_ols <- lm(y ~ x1 + x2, data = df)
tidy(m_ols)

m_naive <- lm(y ~ x1, data = df)
tidy(m_naive)

df_cp <- df %>%
  mutate(x2_fixed = mean(x2))

ggplot(df_cp, aes(x = x1, y = y)) +
  geom_point(alpha = 0.4) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = COL_TREATED
  ) +
  labs(
    title = "Ceteris paribus effect of x1",
    subtitle = "Holding x2 constant at its mean"
  )

coef_compare <- tibble(
  model = c("Naive (y ~ x1)", "Multiple OLS (y ~ x1 + x2)"),
  beta_x1 = c(coef(m_naive)["x1"], coef(m_ols)["x1"])
)

coef_compare

cat("
TAKEAWAY:
- Multiple regression isolates ceteris paribus effects.
- Omitting relevant variables leads to biased coefficients.
")

