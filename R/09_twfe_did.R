
# =========================================================
# Chapter: Two-Way Fixed Effects DiD
# Goal: Causal inference with panel data
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

# ---------------------------------------------------------
# Simulate panel DiD data
# ---------------------------------------------------------

n_individuals <- 100
n_periods <- 6

id <- rep(1:n_individuals, each = n_periods)
time <- rep(1:n_periods, times = n_individuals)

# Treatment group (time-invariant)
treat <- rep(rbinom(n_individuals, 1, 0.5), each = n_periods)

# Post-treatment indicator
post <- ifelse(time >= 4, 1, 0)

# Individual FE
alpha_i <- rep(rnorm(n_individuals, mean = 2, sd = 1),
               each = n_periods)

# Time FE
lambda_t <- rep(seq(0, 1.5, length.out = n_periods),
                times = n_individuals)

# True treatment effect
tau <- 2.0

# Outcome
y <- 5 +
     tau * treat * post +
     alpha_i +
     lambda_t +
     rnorm(n_individuals * n_periods, sd = 1)

df <- tibble(id, time, y, treat, post)

# ---------------------------------------------------------
# TWFE DiD model
# ---------------------------------------------------------
# Note: factor(id) absorbs treat main effect (treat is time-invariant).
#       factor(time) absorbs post main effect (post is time-specific).
#       Only treat:post is separately identified.

m_twfe <- lm(
  y ~ treat:post + factor(id) + factor(time),
  data = df
)

cat('\n--- Two-Way Fixed Effects DiD ---\n')
tidy(m_twfe) %>%
  filter(term == 'treat:post')

cat('
INTERPRETATION:
- treat:post is the DiD causal effect.
- factor(id) controls for individual heterogeneity (absorbs treat main effect).
- factor(time) controls for common time shocks (absorbs post main effect).
')

# ---------------------------------------------------------
# Parallel trends visualization
# ---------------------------------------------------------

avg_trends <- df %>%
  group_by(treat, time) %>%
  summarise(mean_y = mean(y), .groups = 'drop')

p1 <- ggplot(avg_trends,
             aes(x = time, y = mean_y,
                 color = factor(treat),
                 group = treat)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed',
             color = COL_ERROR) +
  labs(
    title = 'Two-Way Fixed Effects DiD',
    subtitle = 'Parallel trends before treatment',
    x = 'Time',
    y = 'Average outcome',
    color = 'Treatment'
  )

print(p1)

cat('
TAKEAWAY:
- TWFE combines DiD with individual and time fixed effects.
- treat main effect is absorbed by individual FE.
- post main effect is absorbed by time FE.
- Only treat:post is the identified DiD estimand.
- This is the workhorse model in applied economics.
')