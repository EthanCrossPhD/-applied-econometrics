
# =========================================================
# Appendix: Sun–Abraham Event Study
# Goal: Interaction-weighted event study
# =========================================================

source('R/00_global_setup.R')

# ---- REQUIRED PACKAGE ----
# install.packages('fixest')
library(fixest)

set.seed(123)

# ---------------------------------------------------------
# Simulate staggered panel data
# ---------------------------------------------------------

n <- 300
t <- 8

id <- rep(1:n, each = t)
time <- rep(1:t, times = n)

# Treatment cohort
g <- sample(c(3, 5, 7, NA), n, replace = TRUE)
group <- rep(g, each = t)

treated <- ifelse(!is.na(group) & time >= group, 1, 0)

# Event time
event_time <- time - group

# Outcome
y <- 5 +
     0.5 * event_time * treated +
     rnorm(n * t)

df <- tibble(id, time, y, treated, group)

# ---------------------------------------------------------
# Sun–Abraham estimator
# ---------------------------------------------------------

m_sa <- feols(
  y ~ sunab(group, time) | id + time,
  data = df
)

summary(m_sa)

# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------

p1 <- iplot(
  m_sa,
  main = 'Sun–Abraham Event Study',
  xlab = 'Event time',
  ylab = 'Effect on y'
)

print(p1)

cat('
TAKEAWAY:
- Sun–Abraham fixes TWFE event studies.
- No treated units used as controls.
- Safe dynamic effects with FE syntax.
')

