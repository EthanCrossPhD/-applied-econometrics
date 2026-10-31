
# =========================================================
# Chapter: Callaway-Sant'Anna DiD
# Goal: Modern DiD with staggered treatment
# =========================================================

source('R/00_global_setup.R')

# ---- REQUIRED PACKAGE ----
# install.packages('did')
library(did)

set.seed(123)

# ---------------------------------------------------------
# Simulate staggered panel data
# ---------------------------------------------------------
# Note: DGP matches 11_problems_twfe.R intentionally.
# Same heterogeneous effects allow direct comparison of
# TWFE (biased) vs Callaway-Sant'Anna (correct).

n <- 300
t <- 8

id <- rep(1:n, each = t)
time <- rep(1:t, times = n)

# Treatment cohort (0 = never treated)
g <- sample(c(3, 5, 7, 0), n, replace = TRUE)
group <- rep(g, each = t)

treated <- ifelse(group > 0 & time >= group, 1, 0)

# Heterogeneous treatment effects (grow over time, same as script 11)
tau <- ifelse(group > 0 & time >= group,
              0.5 * (time - group + 1),
              0)

# Outcome
y <- 5 +
     tau +
     rnorm(n * t)

df <- tibble(
  id    = id,
  time  = time,
  y     = y,
  g     = group
)

# ---------------------------------------------------------
# Callaway-Sant'Anna estimator
# ---------------------------------------------------------

att_gt <- att_gt(
  yname   = 'y',
  tname   = 'time',
  idname  = 'id',
  gname   = 'g',
  data    = df,
  panel   = TRUE
)

summary(att_gt)

# ---------------------------------------------------------
# Aggregation: dynamic effects by event time
# ---------------------------------------------------------

agg_dynamic <- aggte(att_gt, type = 'dynamic')
summary(agg_dynamic)

# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------

p1 <- ggdid(agg_dynamic) +
  labs(
    title    = 'Callaway-Sant\'Anna Event Study',
    subtitle = 'Group-time ATT(g,t) — heterogeneous effects recovered'
  )

print(p1)

cat('
TAKEAWAY:
- Same DGP as 11_problems_twfe.R.
- TWFE collapses growing effects to one biased number.
- Callaway-Sant\'Anna recovers the full dynamic profile.
- Not-yet-treated units used as clean controls.
- ATT(g,t) makes identification transparent.
')