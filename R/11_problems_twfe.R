
# =========================================================
# Chapter: Problems with TWFE
# Goal: Understanding biases with staggered treatment
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

# ---------------------------------------------------------
# Simulate staggered adoption
# ---------------------------------------------------------

n_individuals <- 120
n_periods <- 8

id <- rep(1:n_individuals, each = n_periods)
time <- rep(1:n_periods, times = n_individuals)

# Treatment timing (staggered)
treat_time_by_id <- sample(c(3, 5, 7, Inf),
                            n_individuals,
                            replace = TRUE)

treat_time <- rep(treat_time_by_id, each = n_periods)

treat <- ifelse(time >= treat_time, 1, 0)

# Individual and time FE
alpha_i <- rep(rnorm(n_individuals, mean = 2, sd = 1),
               each = n_periods)
lambda_t <- rep(seq(0, 1.5, length.out = n_periods),
                times = n_individuals)

# Heterogeneous treatment effects (grow over time)
tau <- ifelse(time >= treat_time,
              0.5 * (time - treat_time + 1),
              0)

# Outcome
y <- 5 +
     tau * treat +
     alpha_i +
     lambda_t +
     rnorm(n_individuals * n_periods, sd = 1)

# Note: tau saved in df so we can plot true treatment effects
df <- tibble(id, time, y, treat, treat_time, tau)

# ---------------------------------------------------------
# TWFE regression
# ---------------------------------------------------------

m_twfe <- lm(
  y ~ treat + factor(id) + factor(time),
  data = df
)

cat('\n--- TWFE with staggered adoption ---\n')
tidy(m_twfe) %>% filter(term == 'treat')

cat('
INTERPRETATION:
- TWFE estimates a weighted average of many DiD comparisons.
- With staggered treatment and dynamic effects, bias can arise.
- TWFE returns a single number where the truth is a distribution.
')

# ---------------------------------------------------------
# True average treatment effect by relative time
# ---------------------------------------------------------
# Fix: plot mean(tau) not mean(y).
# mean(y) reflects outcome levels, not treatment effects.
# mean(tau) shows the true heterogeneous effect by event time.

df_rel <- df %>%
  mutate(event_time = time - treat_time) %>%
  filter(is.finite(event_time), treat == 1)

avg_tau <- df_rel %>%
  group_by(event_time) %>%
  summarise(mean_tau = mean(tau), .groups = 'drop')

# TWFE single estimate for reference line
twfe_est <- tidy(m_twfe) %>%
  filter(term == 'treat') %>%
  pull(estimate)

# ---------------------------------------------------------
# Visualization
# ---------------------------------------------------------

p1 <- ggplot(avg_tau,
             aes(x = event_time, y = mean_tau)) +
  geom_line(linewidth = 1.2, color = COL_TREATED) +
  geom_point(size = 3, color = COL_TREATED) +
  geom_hline(yintercept = twfe_est,
             linetype = 'dashed',
             color = COL_ERROR) +
  geom_vline(xintercept = 0,
             linetype = 'dotted') +
  annotate('text',
           x = max(avg_tau$event_time) - 0.5,
           y = twfe_est + 0.1,
           label = paste0('TWFE = ', round(twfe_est, 2)),
           color = COL_ERROR,
           size = 3.5) +
  labs(
    title = 'Staggered Treatment: True Effects vs TWFE Estimate',
    subtitle = 'True tau grows over time; TWFE collapses to one number',
    x = 'Event time (periods since treatment)',
    y = 'Average treatment effect (tau)'
  )

print(p1)

cat('
TAKEAWAY:
- With staggered adoption, treatment effects grow over time.
- TWFE returns a single weighted average of all these effects.
- Some weights can be negative (already-treated used as controls).
- Event Study or modern DiD estimators are preferred.
')