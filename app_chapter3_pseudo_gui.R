####################################################
# Applied Econometrics Studio
# Chapter 3 — Simple OLS (Pseudo-GUI)
####################################################

# Clean session feel
graphics.off()

# Load backend
source("R/simulate_chapter3_data.R")
source("R/run_simple_ols.R")

# --------------------------------------------------
# Fake GUI layout
# --------------------------------------------------

layout(
  matrix(
    c(1, 1,
      2, 3,
      4, 4),
    nrow = 3,
    byrow = TRUE
  ),
  heights = c(1, 6, 2)
)

# --------------------------------------------------
# Panel 1 — Header
# --------------------------------------------------

par(mar = c(1, 1, 2, 1))
plot.new()
rect(0, 0, 1, 1, col = "grey90", border = NA)
text(
  0.5, 0.6,
  "Applied Econometrics Studio",
  cex = 1.6, font = 2
)
text(
  0.5, 0.3,
  "Chapter 3 — Simple Linear Regression (OLS)",
  cex = 1.1
)

# --------------------------------------------------
# Panel 2 — Controls (fake)
# --------------------------------------------------

par(mar = c(2, 2, 2, 2))
plot.new()
rect(0, 0, 1, 1, col = "grey95", border = NA)

text(0.05, 0.9, "Controls", adj = 0, font = 2)

Y_var <- "y"
X_var <- "x"

text(0.05, 0.75, paste("Dependent variable (Y):", Y_var), adj = 0)
text(0.05, 0.6,  paste("Independent variable (X):", X_var), adj = 0)

rect(0.25, 0.2, 0.75, 0.35, col = "steelblue", border = NA)
text(0.5, 0.275, "RUN REGRESSION", col = "white", font = 2)

# --------------------------------------------------
# Run model (backend)
# --------------------------------------------------

df  <- simulate_chapter3_data()
res <- run_simple_ols(df, y = Y_var, x = X_var)

# --------------------------------------------------
# Panel 3 — Plot
# --------------------------------------------------

par(mar = c(4, 4, 2, 1))
plot(
  df[[X_var]], df[[Y_var]],
  pch = 19,
  col = "steelblue",
  xlab = X_var,
  ylab = Y_var,
  main = "Model Fit"
)
abline(res$model, col = "firebrick", lwd = 2)

# --------------------------------------------------
# Panel 4 — Interpretation
# --------------------------------------------------

par(mar = c(1, 1, 1, 1))
plot.new()
rect(0, 0, 1, 1, col = "grey90", border = NA)

text(0.02, 0.65, "Interpretation:", adj = 0, font = 2)
text(
  0.02, 0.35,
  res$interpretation,
  adj = 0,
  cex = 1.1
)