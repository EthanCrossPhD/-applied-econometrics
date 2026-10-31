
# =========================================================
# Chapter: Panel Data & Fixed Effects
# Goal: Controlling for unobserved heterogeneity
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

# ---------------------------------------------------------
# Simulate panel data
# ---------------------------------------------------------

n_individuals <- 100
n_periods <- 5

id <- rep(1:n_individuals, each = n_periods)
time <- rep(1:n_periods, times = n_individuals)

# Individual fixed effects (unobserved)
alpha_i <- rep(rnorm(n_individuals, mean = 2, sd = 1),
               each = n_periods)

# Time-varying regressor
x <- runif(n_individuals * n_periods, 0, 10)

# Outcome
y <- 1 + 1.5 * x + alpha_i + rnorm(n_individuals * n_periods, sd = 1)

df <- tibble(id, time, y, x)

# ---------------------------------------------------------
# Pooled OLS (ignores FE)
# ---------------------------------------------------------

m_pooled <- lm(y ~ x, data = df)

cat('\n--- Pooled OLS ---\n')
tidy(m_pooled)

# ---------------------------------------------------------
# Fixed Effects via dummy variables
# ---------------------------------------------------------

m_fe <- lm(y ~ x + factor(id), data = df)

cat('\n--- Fixed Effects (via factor(id)) ---\n')
tidy(m_fe)

cat('
INTERPRETATION:
- Pooled OLS ignores unobserved heterogeneity.
- Fixed Effects control for all time-invariant individual traits.
- Identification comes from within-individual variation.
')

# ---------------------------------------------------------
# Compare coefficients
# ---------------------------------------------------------

coef_compare <- tibble(
  model = c('Pooled OLS', 'Fixed Effects'),
  beta_x = c(coef(m_pooled)['x'], coef(m_fe)['x'])
)

coef_compare

cat('
TAKEAWAY:
- FE estimates are based on within-unit changes.
- Time-invariant regressors are absorbed by fixed effects.
- FE can be implemented with simple dummy variables.
')

