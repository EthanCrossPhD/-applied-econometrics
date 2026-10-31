####################################################
# Chapter 4 — Multiple OLS backend
####################################################

run_multiple_ols <- function(df) {

  Y_var  <- "y"
  X_vars <- c("x1", "x2")

  fml <- as.formula(
    paste(Y_var, "~", paste(X_vars, collapse = " + "))
  )

  model <- lm(fml, data = df)

  coefs <- coef(model)

  interpretation <- paste0(
    "This multiple regression estimates the effect of x1 on y ",
    "while holding x2 constant.\n\n",
    "A one-unit increase in x1 is associated with an average change of ",
    round(coefs["x1"], 3),
    " units in y, ceteris paribus.\n\n",
    "The model explains ",
    round(summary(model)$r.squared * 100, 1),
    "% of the variation in y."
  )

  list(
    model = model,
    interpretation = interpretation,
    y = Y_var,
    x = X_vars
  )
}