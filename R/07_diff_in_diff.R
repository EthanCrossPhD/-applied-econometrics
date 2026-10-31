
# =========================================================
# Chapter: Difference-in-Differences
# Goal: Causal inference with panel-style data
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 500

# Treatment group indicator
treat <- rbinom(n, 1, 0.5)

# Time indicator (0 = pre, 1 = post)
post <- rbinom(n, 1, 0.5)

# True treatment effect
tau <- 2.0

# Data generating process
y <- 5 +
     1.0 * treat +
     0.5 * post +
     tau * treat * post +
     rnorm(n, sd = 1)

df <- tibble(y, treat, post)

# ---------------------------------------------------------
# DiD model
# ---------------------------------------------------------

m_did <- lm(y ~ treat * post, data = df)

cat('\n--- Difference-in-Differences model ---\n')
tidy(m_did)

cat('
INTERPRETATION:
- beta_treat      : baseline difference between groups
- beta_post       : common time trend
- beta_treat:post : DiD estimator (causal effect)
')

# ---------------------------------------------------------
# Group means (manual DiD)
# ---------------------------------------------------------

means <- df %>%
  group_by(treat, post) %>%
  summarise(mean_y = mean(y), .groups = 'drop')

means

did_manual <- (means$mean_y[means$treat == 1 & means$post == 1] -
               means$mean_y[means$treat == 1 & means$post == 0]) -
              (means$mean_y[means$treat == 0 & means$post == 1] -
               means$mean_y[means$treat == 0 & means$post == 0])

cat('\nManual DiD estimate:', did_manual, '\n')

# ---------------------------------------------------------
# Visualization: DiD plot
# ---------------------------------------------------------

p1 <- ggplot(means,
             aes(x = post, y = mean_y,
                 color = factor(treat),
                 group = treat)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c('Pre', 'Post')) +
  labs(
    title = 'Difference-in-Differences',
    subtitle = 'Parallel trends assumption',
    x = 'Time',
    y = 'Average outcome',
    color = 'Treatment'
  )

print(p1)

cat('
TAKEAWAY:
- DiD is an interaction between Treatment and Post.
- The causal effect is the difference in changes.
- Validity relies on the parallel trends assumption.
')

