
# =========================================================
# Chapter generator — Applied Econometrics Studio
# =========================================================

dir.create("R", showWarnings = FALSE)

# =========================================================
# Generate: Multiple OLS
# (unchanged — exactly as provided)
# =========================================================

multiple_ols_code <- "
# =========================================================
# Chapter: Multiple OLS
# Goal: Estimating ceteris paribus effects
# =========================================================

source(\"R/00_global_setup.R\")

set.seed(123)

n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
u  <- rnorm(n, sd = 1)

y <- 1 + 2*x1 - 1.5*x2 + u

df <- tibble(y, x1, x2)

m_ols <- lm(y ~ x1 + x2, data = df)
tidy(m_ols)

m_naive <- lm(y ~ x1, data = df)
tidy(m_naive)

df_cp <- df %>%
  mutate(x2_fixed = mean(x2))

ggplot(df_cp, aes(x = x1, y = y)) +
  geom_point(alpha = 0.4) +
  geom_smooth(
    method = \"lm\",
    formula = y ~ x,
    color = COL_TREATED
  ) +
  labs(
    title = \"Ceteris paribus effect of x1\",
    subtitle = \"Holding x2 constant at its mean\"
  )

coef_compare <- tibble(
  model = c(\"Naive (y ~ x1)\", \"Multiple OLS (y ~ x1 + x2)\"),
  beta_x1 = c(coef(m_naive)[\"x1\"], coef(m_ols)[\"x1\"])
)

coef_compare

cat(\"\nTAKEAWAY:\n- Multiple regression isolates ceteris paribus effects.\n- Omitting relevant variables leads to biased coefficients.\n\")
"

writeLines(multiple_ols_code, con = "R/01_multiple_ols.R")
message("✅ Generated: R/01_multiple_ols.R")

# =========================================================
# Generate: Heteroskedasticity
# =========================================================

heterosked_code <- "
# =========================================================
# Chapter: Heteroskedasticity
# Goal: Understanding heteroskedastic errors
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 300
x <- runif(n, 0, 10)

# Heteroskedastic errors: variance increases with x
u <- rnorm(n, sd = 0.5 + 0.3 * x)

y <- 1 + 2 * x + u

df <- tibble(y, x)

# ---- OLS model ----
m_ols <- lm(y ~ x, data = df)
tidy(m_ols)

# ---- Residuals ----
df_resid <- df %>%
  mutate(
    resid = resid(m_ols),
    fitted = fitted(m_ols)
  )

# ---- Residual plot ----
ggplot(df_resid, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.4, color = COL_ERROR) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    title = 'Heteroskedasticity in OLS residuals',
    subtitle = 'Error variance increases with the level of the fitted value',
    x = 'Fitted values',
    y = 'Residuals'
  )

cat('
TAKEAWAY:
- Heteroskedasticity violates the constant variance assumption.
- OLS coefficients remain unbiased, but standard errors are unreliable.
')
"

writeLines(heterosked_code, con = "R/02_heteroskedasticity.R")
message("✅ Generated: R/02_heteroskedasticity.R")




# =========================================================
# Generate: Autocorrelation
# =========================================================

autocorr_code <- "
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
"

writeLines(autocorr_code, con = "R/03_autocorrelation.R")
message("✅ Generated: R/03_autocorrelation.R")


# =========================================================
# Generate: HC & HAC Standard Errors
# =========================================================

hc_hac_code <- "
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

cat('\n--- Heteroskedasticity: OLS vs HC ---\n')
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

cat('\n--- Autocorrelation: OLS vs HAC ---\n')
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
"

writeLines(hc_hac_code, con = "R/04_hc_hac.R")
message("✅ Generated: R/04_hc_hac.R")

# =========================================================
# Generate: Functional Forms
# =========================================================

functional_forms_code <- "
# =========================================================
# Chapter: Functional Forms
# Goal: Understanding transformations and nonlinearities
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 300
x <- runif(n, 1, 10)

# True nonlinear relationship
y <- 1 + 2 * log(x) + rnorm(n, sd = 0.3)

df <- tibble(y, x)

# ---------------------------------------------------------
# Linear vs Log model
# ---------------------------------------------------------

m_linear <- lm(y ~ x, data = df)
m_log    <- lm(y ~ log(x), data = df)

cat('\\n--- Linear vs Log specification ---\\n')
bind_rows(
  tidy(m_linear) %>% mutate(model = 'Linear'),
  tidy(m_log)    %>% mutate(model = 'Log')
)

# ---- Visualization: Linear vs Log ----
p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'lm', formula = y ~ x, color = COL_CONTROL) +
  geom_smooth(method = 'lm', formula = y ~ log(x), color = COL_TREATED) +
  labs(
    title = 'Linear vs Log functional form',
    subtitle = 'True relationship is logarithmic'
  )

print(p1)

# ---------------------------------------------------------
# Polynomial model
# ---------------------------------------------------------

m_quad <- lm(y ~ x + I(x^2), data = df)

cat('\\n--- Quadratic specification ---\\n')
tidy(m_quad)

# ---- Marginal effect ----
df_me <- df %>%
  mutate(
    marginal_effect = coef(m_quad)['x'] +
                      2 * coef(m_quad)['I(x^2)'] * x
  )

p2 <- ggplot(df_me, aes(x = x, y = marginal_effect)) +
  geom_line(color = COL_TREATED) +
  labs(
    title = 'Marginal effect in quadratic model',
    subtitle = 'Effect of x depends on the level of x',
    y = '∂y / ∂x'
  )

print(p2)

cat('
TAKEAWAY:
- Functional form matters for interpretation and fit.
- Log models capture diminishing marginal effects.
- In nonlinear models, marginal effects depend on x.
')
"

writeLines(functional_forms_code, con = "R/05_functional_forms.R")
message("✅ Generated: R/05_functional_forms.R")

# =========================================================
# Generate: Interactions
# =========================================================

interactions_code <- "
# =========================================================
# Chapter: Interactions
# Goal: Understanding interaction effects
# =========================================================

source('R/00_global_setup.R')

set.seed(123)

n <- 400

# Binary group indicator
z <- rbinom(n, 1, 0.5)

# Continuous regressor
x <- runif(n, 0, 10)

# True DGP with interaction
y <- 1 +
     1.5 * x +
     2.0 * z +
     (-0.8) * x * z +
     rnorm(n, sd = 1)

df <- tibble(y, x, z)

# ---------------------------------------------------------
# Interaction model
# ---------------------------------------------------------

m_int <- lm(y ~ x * z, data = df)

cat('\\n--- Interaction model: y ~ x * z ---\\n')
tidy(m_int)

# ---------------------------------------------------------
# Interpretation
# ---------------------------------------------------------
cat('
INTERPRETATION:
- beta_x     : effect of x when z = 0
- beta_z     : level difference when x = 0
- beta_x:z   : difference in slope between z = 1 and z = 0
')

# ---------------------------------------------------------
# Marginal effect of x
# ---------------------------------------------------------

df_me <- tibble(
  z = c(0, 1),
  marginal_effect = coef(m_int)['x'] +
                    coef(m_int)['x:z'] * z
)

df_me

# ---------------------------------------------------------
# Visualization: different slopes
# ---------------------------------------------------------

p1 <- ggplot(df, aes(x = x, y = y, color = factor(z))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(
    title = 'Interaction between x and group z',
    subtitle = 'Effect of x differs by group',
    color = 'Group z'
  )

print(p1)

cat('
TAKEAWAY:
- Interactions allow slopes to differ across groups.
- The effect of x depends on the value of z.
- Always interpret interactions via marginal effects.
')
"

writeLines(interactions_code, con = "R/06_interactions.R")
message("✅ Generated: R/06_interactions.R")


# =========================================================
# Generate: Difference-in-Differences
# =========================================================

did_code <- "
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

cat('\\n--- Difference-in-Differences model ---\\n')
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

cat('\\nManual DiD estimate:', did_manual, '\\n')

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
"

writeLines(did_code, con = "R/07_diff_in_diff.R")
message("✅ Generated: R/07_diff_in_diff.R")

# =========================================================
# Generate: Panel Data & Fixed Effects
# =========================================================

panel_fe_code <- "
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

cat('\\n--- Pooled OLS ---\\n')
tidy(m_pooled)

# ---------------------------------------------------------
# Fixed Effects via dummy variables
# ---------------------------------------------------------

m_fe <- lm(y ~ x + factor(id), data = df)

cat('\\n--- Fixed Effects (via factor(id)) ---\\n')
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
"

writeLines(panel_fe_code, con = "R/08_panel_fixed_effects.R")
message("✅ Generated: R/08_panel_fixed_effects.R")

# =========================================================
# Generate: Two-Way Fixed Effects DiD
# =========================================================

twfe_did_code <- "
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

m_twfe <- lm(
  y ~ treat * post + factor(id) + factor(time),
  data = df
)

cat('\\n--- Two-Way Fixed Effects DiD ---\\n')
tidy(m_twfe)

cat('
INTERPRETATION:
- treat:post is the DiD causal effect.
- factor(id) controls for individual heterogeneity.
- factor(time) controls for common time shocks.
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
- Identification relies on parallel trends.
- This is the workhorse model in applied economics.
')
"

writeLines(twfe_did_code, con = "R/09_twfe_did.R")
message("✅ Generated: R/09_twfe_did.R")



# =========================================================
# Generate: Event Study
# =========================================================

event_study_code <- "
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

cat('\\n--- Event Study (reference period t = -1) ---\\n')
tidy(m_es)

# ---------------------------------------------------------
# Extract event-time coefficients
# ---------------------------------------------------------

es_coefs <- tidy(m_es) %>%
  filter(str_detect(term, 'event_time_f')) %>%
  mutate(
    event_time = as.numeric(
      str_extract(term, '-?\\\\d+')
    )
  )

# ---------------------------------------------------------
# Visualization
# ---------------------------------------------------------

p1 <- ggplot(es_coefs,
             aes(x = event_time, y = estimate)) +
  geom_point(size = 3, color = COL_TREATED) +
  geom_line(linewidth = 1.2, color = COL_TREATED) +
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
- Pre-treatment coefficients should be near zero.
- Violations indicate non-parallel trends.
')
"

writeLines(event_study_code, con = "R/10_event_study.R")
message("✅ Generated: R/10_event_study.R")


# =========================================================
# Generate: Problems with TWFE
# =========================================================

problems_twfe_code <- "
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

# Heterogeneous treatment effects
tau <- ifelse(time >= treat_time,
              0.5 * (time - treat_time + 1),
              0)

# Outcome
y <- 5 +
     tau * treat +
     alpha_i +
     lambda_t +
     rnorm(n_individuals * n_periods, sd = 1)

df <- tibble(id, time, y, treat, treat_time)

# ---------------------------------------------------------
# TWFE regression
# ---------------------------------------------------------

m_twfe <- lm(
  y ~ treat + factor(id) + factor(time),
  data = df
)

cat('\\n--- TWFE with staggered adoption ---\\n')
tidy(m_twfe)

cat('
INTERPRETATION:
- TWFE estimates a weighted average of many DiD comparisons.
- With staggered treatment and dynamic effects, bias can arise.
')

# ---------------------------------------------------------
# Average treatment effect by relative time
# ---------------------------------------------------------

df_rel <- df %>%
  mutate(event_time = time - treat_time) %>%
  filter(is.finite(event_time))

avg_tau <- df_rel %>%
  group_by(event_time) %>%
  summarise(mean_tau = mean(y), .groups = 'drop')

# ---------------------------------------------------------
# Visualization
# ---------------------------------------------------------

p1 <- ggplot(avg_tau,
             aes(x = event_time, y = mean_tau)) +
  geom_line(linewidth = 1.2, color = COL_TREATED) +
  geom_point(size = 3, color = COL_TREATED) +
  geom_vline(xintercept = 0,
             linetype = 'dashed',
             color = COL_ERROR) +
  labs(
    title = 'Staggered Treatment Effects',
    subtitle = 'Treatment effects grow over time',
    x = 'Event time',
    y = 'Average outcome'
  )

print(p1)

cat('
TAKEAWAY:
- With staggered adoption, TWFE mixes different comparisons.
- Dynamic treatment effects can bias TWFE estimates.
- Event Study or modern DiD estimators are preferred.
')
"

writeLines(problems_twfe_code, con = "R/11_problems_twfe.R")
message("✅ Generated: R/11_problems_twfe.R")

# =========================================================
# Generate: Random Effects & Hausman Test
# =========================================================

re_hausman_code <- "
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

cat('\\n--- Fixed Effects ---\\n')
tidy(m_fe)

# ---------------------------------------------------------
# Random Effects (Mundlak formulation)
# ---------------------------------------------------------

df <- df %>%
  group_by(id) %>%
  mutate(x_bar = mean(x)) %>%
  ungroup()

m_re <- lm(y ~ x + x_bar, data = df)

cat('\\n--- Random Effects (Mundlak) ---\\n')
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
"

writeLines(re_hausman_code, con = "R/12_random_effects_hausman.R")
message("✅ Generated: R/12_random_effects_hausman.R")


# =========================================================
# Generate: Modern DiD (Conceptual)
# =========================================================

modern_did_code <- "
# =========================================================
# Chapter: Modern Difference-in-Differences
# Goal: Understanding modern DiD estimators
# =========================================================

source('R/00_global_setup.R')

cat('
--- Why TWFE fails with staggered treatment ---

Problem:
- Treatment occurs at different times.
- Treatment effects change over time.
- TWFE averages many DiD comparisons.
- Some comparisons use already-treated units as controls.

Result:
- Estimates can be biased.
- Weights can be negative.
')

cat('
--- Callaway–Sant’Anna (2021) ---

Core idea:
- Estimate treatment effects by:
  * treatment cohort (g)
  * calendar time (t)

ATT(g,t):
- Compare group treated at time g
- To units not yet treated at time t

Advantages:
- Clean control group
- Transparent identification
- Aggregation is explicit
')

cat('
--- Sun–Abraham (2021) ---

Core idea:
- Modify Event Study
- Use interactions that avoid forbidden comparisons

Key feature:
- No treated units used as controls
- Event-time coefficients are interpretable

Advantages:
- Compatible with TWFE framework
- Safe event-study plots
')

cat('
--- Practical Guidance ---

Use TWFE when:
- Treatment timing is the same for everyone
- Or effects are homogeneous

Use Modern DiD when:
- Treatment is staggered
- Effects evolve over time

Rule of thumb:
- If staggered adoption → TWFE is risky
- Prefer Callaway–Sant’Anna or Sun–Abraham
')

cat('
TAKEAWAY:
- Modern DiD fixes the core problem of TWFE.
- Identification must be explicit.
- Today, modern DiD is the applied standard.
')
"

writeLines(modern_did_code, con = "R/13_modern_did_conceptual.R")
message("✅ Generated: R/13_modern_did_conceptual.R")


# =========================================================
# Generate: Callaway–Sant’Anna DiD
# =========================================================

cs_did_code <- "
# =========================================================
# Appendix: Callaway–Sant'Anna DiD
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

n <- 300
t <- 8

id <- rep(1:n, each = t)
time <- rep(1:t, times = n)

# Treatment cohort
g <- sample(c(3, 5, 7, 0), n, replace = TRUE)
g <- ifelse(g == 0, 0, g)
group <- rep(g, each = t)

treated <- ifelse(group > 0 & time >= group, 1, 0)

# Outcome
y <- 5 +
     2 * treated +
     rnorm(n * t)

df <- tibble(
  id = id,
  time = time,
  y = y,
  g = group
)

# ---------------------------------------------------------
# Callaway–Sant'Anna estimator
# ---------------------------------------------------------

att_gt <- att_gt(
  yname = 'y',
  tname = 'time',
  idname = 'id',
  gname = 'g',
  data = df,
  panel = TRUE
)

summary(att_gt)

# ---------------------------------------------------------
# Aggregation
# ---------------------------------------------------------

agg_dynamic <- aggte(att_gt, type = 'dynamic')
summary(agg_dynamic)

# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------

p1 <- ggdid(agg_dynamic) +
  labs(
    title = 'Callaway–Sant’Anna Event Study',
    subtitle = 'Group-time ATT(g,t)'
  )

print(p1)

cat('
TAKEAWAY:
- Callaway–Sant’Anna uses not-yet-treated units as controls.
- ATT(g,t) makes identification explicit.
- This is the modern DiD benchmark.
')
"

writeLines(cs_did_code, con = 'R/14_callaway_santanna.R')
message('✅ Generated: R/14_callaway_santanna.R')


# =========================================================
# Generate: Sun–Abraham Event Study
# =========================================================

sa_did_code <- "
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
"

writeLines(sa_did_code, con = 'R/15_sun_abraham.R')
message('✅ Generated: R/15_sun_abraham.R')