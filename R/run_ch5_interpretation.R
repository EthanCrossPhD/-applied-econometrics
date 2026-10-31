####################################################
# Chapter 5 — Interpretation & Marginal Effects
####################################################

run_ch5_interpretation <- function(df) {

  model <- lm(y ~ x1 + x2, data = df)

  b1 <- coef(model)["x1"]

  interpretation <- paste0(
    "The coefficient on x1 represents the marginal effect of x1 on y.\n\n",
    "A one-unit increase in x1 increases y by ",
    round(b1, 3),
    " units on average, holding x2 constant.\n\n",
    "This is a marginal effect, not a total or predicted change."
  )

  list(
    model = model,
    b1 = b1,
    interpretation = interpretation
  )
}