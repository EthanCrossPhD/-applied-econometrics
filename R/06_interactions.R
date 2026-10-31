
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

cat('\n--- Interaction model: y ~ x * z ---\n')
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

