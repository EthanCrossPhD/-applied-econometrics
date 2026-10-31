
# =========================================================
# Chapter: Event Study
# Goal: Dynamic treatment effects and pre-trend testing
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

# ---------------------------------------------------------
# Simulate panel data with treatment timing
# ---------------------------------------------------------

n_individuals <- 100
n_periods <- 8

id <- rep(1:n_individuals, each = n_periods)
time <- rep(1:n_periods, times = n_individuals)

# Treatment group
treat <- rep(rbinom(n_individuals, 1, 0.5), each = n_periods)

# Treatment starts at time = 5
treat_time <- 5

# Event time
event_time <- time - treat_time

# Individual and time FE
alpha_i <- rep(rnorm(n_individuals, mean = 2, sd = 1),
               each = n_periods)
lambda_t <- rep(seq(0, 1.5, length.out = n_periods),
                times = n_individuals)

# Dynamic treatment effects
tau <- ifelse(event_time >= 0, 0.5 * event_time, 0)

# Outcome
y <- 5 +
     tau * treat +
     alpha_i +
     lambda_t +
     rnorm(n_individuals * n_periods, sd = 1)

df <- tibble(id, time, y, treat, event_time)

# ---------------------------------------------------------
# Event Study regression
# ---------------------------------------------------------

df <- df %>%
  mutate(event_time_f = factor(event_time))

# Drop reference period (t = -1)
df$event_time_f <- relevel(df$event_time_f, ref = '-1')

m_es <- lm(
  y ~ event_time_f * treat + factor(id) + factor(time),
  data = df
)

cat('\n--- Event Study (reference period t = -1) ---\n')
tidy(m_es)

# ---------------------------------------------------------
# Extract event-time coefficients (interaction terms only)
# ---------------------------------------------------------
# Fix: filter to event_time_f*:treat interactions only.
# The main effects of event_time_f levels are not event-study coefficients.

es_coefs <- tidy(m_es, conf.int = TRUE) %>%
  filter(str_detect(term, 'event_time_f.*:treat')) %>%
  mutate(
    event_time = as.numeric(
      str_extract(term, '-?\\d+')
    )
  )

# ---------------------------------------------------------
# Visualization with confidence intervals
# ---------------------------------------------------------

p1 <- ggplot(es_coefs,
             aes(x = event_time, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = COL_TREATED) +
  geom_line(linewidth = 1.2, color = COL_TREATED) +
  geom_point(size = 3, color = COL_TREATED) +
  geom_hline(yintercept = 0,
             linetype = 'dashed',
             color = COL_ERROR) +
  geom_vline(xintercept = -0.5,
             linetype = 'dotted') +
  labs(
    title = 'Event Study: Dynamic Treatment Effects',
    subtitle = 'Pre-period coefficients test parallel trends',
    x = 'Event time',
    y = 'Effect on y'
  )

print(p1)

cat('
TAKEAWAY:
- Event Study shows how treatment effects evolve over time.
- Pre-treatment coefficients should be near zero (CI band covers zero).
- Post-treatment coefficients grow because tau = 0.5 * event_time.
- Violations of flat pre-trends indicate non-parallel trends.
')