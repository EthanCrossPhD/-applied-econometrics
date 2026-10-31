
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

cat('\n--- Linear vs Log specification ---\n')
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

cat('\n--- Quadratic specification ---\n')
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

