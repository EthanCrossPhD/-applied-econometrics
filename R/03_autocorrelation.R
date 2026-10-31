
# =========================================================
# Chapter: Autocorrelation
# Goal: Understanding serially correlated errors
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 300
time <- 1:n
x <- rnorm(n)

# AR(1) error process
u <- numeric(n)
u[1] <- rnorm(1)
rho <- 0.7

for (t in 2:n) {
  u[t] <- rho * u[t - 1] + rnorm(1)
}

y <- 1 + 2 * x + u

df <- tibble(time, y, x)

# ---- OLS model ----
m_ols <- lm(y ~ x, data = df)
tidy(m_ols)

# ---- Residuals over time ----
df_resid <- df %>%
  mutate(resid = resid(m_ols))

ggplot(df_resid, aes(x = time, y = resid)) +
  geom_line(color = COL_ERROR) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    title = 'Autocorrelated residuals over time',
    subtitle = 'Positive serial correlation (AR(1) errors)',
    x = 'Time',
    y = 'Residuals'
  )

cat('
TAKEAWAY:
- Autocorrelation violates the independence assumption.
- OLS coefficients remain unbiased, but standard errors are unreliable.
- Residuals exhibit systematic patterns over time.
')

