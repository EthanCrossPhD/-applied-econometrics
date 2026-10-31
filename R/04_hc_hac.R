
# =========================================================
# Chapter: HC & HAC Standard Errors
# Goal: Correcting inference under heteroskedasticity and autocorrelation
# =========================================================

source('R/00_global_setup.R')

library(sandwich)
library(lmtest)

set.seed(123)

# ---------------------------------------------------------
# Part 1: Heteroskedasticity (HC)
# ---------------------------------------------------------

n <- 300
x <- runif(n, 0, 10)
u <- rnorm(n, sd = 0.5 + 0.3 * x)
y <- 1 + 2 * x + u

df_het <- tibble(y, x)

m_het <- lm(y ~ x, data = df_het)

# Standard OLS SE
ols_se <- tidy(m_het)

# HC (White) SE
hc_se <- tidy(coeftest(m_het, vcov = vcovHC(m_het, type = 'HC1')))

cat('
--- Heteroskedasticity: OLS vs HC ---
')
bind_rows(
  ols_se %>% mutate(se_type = 'OLS'),
  hc_se  %>% mutate(se_type = 'HC')
)

# ---------------------------------------------------------
# Part 2: Autocorrelation (HAC)
# ---------------------------------------------------------

n <- 300
time <- 1:n
x <- rnorm(n)

u <- numeric(n)
u[1] <- rnorm(1)
rho <- 0.7

for (t in 2:n) {
  u[t] <- rho * u[t - 1] + rnorm(1)
}

y <- 1 + 2 * x + u

df_ac <- tibble(time, y, x)

m_ac <- lm(y ~ x, data = df_ac)

# Standard OLS SE
ols_se_ac <- tidy(m_ac)

# HAC (Newey-West) SE
hac_se <- tidy(coeftest(m_ac, vcov = NeweyWest(m_ac)))

cat('
--- Autocorrelation: OLS vs HAC ---
')
bind_rows(
  ols_se_ac %>% mutate(se_type = 'OLS'),
  hac_se    %>% mutate(se_type = 'HAC')
)

cat('
TAKEAWAY:
- Robust standard errors fix inference, not coefficients.
- HC handles heteroskedasticity; HAC handles autocorrelation.
- OLS point estimates remain unchanged.
')

