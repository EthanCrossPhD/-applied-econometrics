
# =========================================================
# Chapter: Heteroskedasticity
# Goal: Understanding heteroskedastic errors
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 300
x <- runif(n, 0, 10)

# Heteroskedastic errors: variance increases with x
u <- rnorm(n, sd = 0.5 + 0.3 * x)

y <- 1 + 2 * x + u

df <- tibble(y, x)

# ---- OLS model ----
m_ols <- lm(y ~ x, data = df)
tidy(m_ols)

# ---- Residuals ----
df_resid <- df %>%
  mutate(
    resid = resid(m_ols),
    fitted = fitted(m_ols)
  )

# ---- Residual plot ----
ggplot(df_resid, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4, color = COL_ERROR) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    title = 'Heteroskedasticity in OLS residuals',
    subtitle = 'Error variance increases with the level of the fitted value',
    x = 'Fitted values',
    y = 'Residuals'
  )

cat('
TAKEAWAY:
- Heteroskedasticity violates the constant variance assumption.
- OLS coefficients remain unbiased, but standard errors are unreliable.
')

