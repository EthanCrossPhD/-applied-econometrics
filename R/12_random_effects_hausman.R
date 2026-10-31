
# =========================================================
# Chapter: Random Effects & Hausman
# Goal: Choosing between FE and RE
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

# ---------------------------------------------------------
# Simulate panel data with correlated effects
# ---------------------------------------------------------

n_individuals <- 100
n_periods <- 6

id <- rep(1:n_individuals, each = n_periods)
time <- rep(1:n_periods, times = n_individuals)

# Individual effect correlated with x
alpha_i_raw <- rnorm(n_individuals)
alpha_i <- rep(alpha_i_raw, each = n_periods)

# Regressor correlated with alpha_i
x <- runif(n_individuals * n_periods, 0, 10) +
     rep(alpha_i_raw, each = n_periods)

# Outcome
y <- 1 + 2 * x + alpha_i + rnorm(n_individuals * n_periods, sd = 1)

df <- tibble(id, time, y, x)

# ---------------------------------------------------------
# Fixed Effects
# ---------------------------------------------------------

m_fe <- lm(y ~ x + factor(id), data = df)

cat('\n--- Fixed Effects ---\n')
tidy(m_fe)

# ---------------------------------------------------------
# Random Effects (Mundlak formulation)
# ---------------------------------------------------------

df <- df %>%
  group_by(id) %>%
  mutate(x_bar = mean(x)) %>%
  ungroup()

m_re <- lm(y ~ x + x_bar, data = df)

cat('\n--- Random Effects (Mundlak) ---\n')
tidy(m_re)

cat('
INTERPRETATION:
- RE assumes no correlation between individual effects and regressors.
- Mundlak test checks this assumption.
')

# ---------------------------------------------------------
# Hausman-style test (intuition)
# ---------------------------------------------------------

hausman_test <- tidy(m_re) %>%
  filter(term == 'x_bar')

hausman_test

cat('
HAUSMAN LOGIC:
- If coefficient on x_bar ≠ 0 → RE assumption violated.
- In that case, FE is consistent and preferred.
')

cat('
TAKEAWAY:
- FE is robust but less efficient.
- RE is efficient but risky.
- Hausman test guides the choice.
- In applied work: FE is usually the safe default.
')

