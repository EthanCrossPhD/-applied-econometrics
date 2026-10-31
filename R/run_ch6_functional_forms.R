####################################################
# Chapter 6 — Functional Forms (BASIC, robust)
####################################################

run_ch6_functional_forms <- function(df) {

  # ------------------------------------------------
  # Safety: ensure positivity for log model
  # ------------------------------------------------
  if (any(df$y <= 0)) {
    df$y <- df$y - min(df$y) + 1
  }

  # ------------------------------------------------
  # Models
  # ------------------------------------------------

  # Linear
  m_linear <- lm(y ~ x1 + x2, data = df)

  # Log-linear (log y)
  m_loglin <- lm(log(y) ~ x1 + x2, data = df)

  # Quadratic
  m_quad <- lm(y ~ x1 + I(x1^2) + x2, data = df)

  # ------------------------------------------------
  # Interpretation text
  # ------------------------------------------------

  interpretation <- paste0(
    "The choice of functional form affects economic interpretation.\n\n",
    "Linear: the marginal effect of x1 is constant.\n",
    "Log-linear: coefficients approximate percentage changes in y.\n",
    "Quadratic: the marginal effect of x1 depends on the level of x1."
  )

  # ------------------------------------------------
  # Return
  # ------------------------------------------------

  list(
    linear = m_linear,
    loglin = m_loglin,
    quad   = m_quad,
    interp = interpretation,
    data   = df
  )
}