####################################################
# Applied Econometrics Studio
# Chapter 3 — Simple Linear Regression (OLS)
####################################################

cat("\n====================================================\n")
cat(" Applied Econometrics Studio\n")
cat(" Chapter 3 — Simple Linear Regression (OLS)\n")
cat("====================================================\n\n")

# --------------------------------------------------
# Load backend
# --------------------------------------------------

source("R/simulate_chapter3_data.R")
source("R/run_simple_ols.R")

cat("Dataset: Simulated teaching data\n\n")

# --------------------------------------------------
# User choices (FAKE GUI CONTROLS)
# --------------------------------------------------

Y_var <- "y"
X_var <- "x"

cat("Dependent variable (Y): ", Y_var, "\n")
cat("Independent variable (X): ", X_var, "\n\n")

cat("[ Estimating model... ]\n")

# --------------------------------------------------
# Run model
# --------------------------------------------------

df  <- simulate_chapter3_data()
res <- run_simple_ols(df, y = Y_var, x = X_var)

cat("[ DONE ]\n\n")

# --------------------------------------------------
# Visual output
# --------------------------------------------------

cat("----------------------------------------------------\n")
cat(" VISUAL OUTPUT\n")
cat("----------------------------------------------------\n")

plot(
  df[[X_var]], df[[Y_var]],
  pch = 19,
  col = "steelblue",
  xlab = X_var,
  ylab = Y_var,
  main = "Simple Linear Regression"
)

abline(res$model, col = "firebrick", lwd = 2)

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

cat("\n----------------------------------------------------\n")
cat(" INTERPRETATION\n")
cat("----------------------------------------------------\n")

cat(res$interpretation, "\n")

cat("\n====================================================\n")
cat(" End of Chapter 3 Prototype\n")
cat("====================================================\n")