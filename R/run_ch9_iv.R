####################################################
# Chapter 9 — Instrumental Variables (BASIC)
####################################################

run_ch9_iv <- function(n = 300) {

  set.seed(777)

  # Instrument
  z <- rnorm(n)

  # Structural error
  u <- rnorm(n)

  # Additional shock
  v <- rnorm(n)

  # Endogenous regressor (correlated with u)
  x <- 0.8 * z + 0.8 * u + v

  # Outcome
  y <- 1 + 1.2 * x + u

  interpretation <- paste0(
    "The regressor x is endogenous because it is correlated with the error term.\n\n",
    "The instrument z affects x but is independent of the structural error.\n\n",
    "IV estimation uses only the variation in x induced by z."
  )

  list(
    data = data.frame(y = y, x = x, z = z),
    interp = interpretation
  )
}