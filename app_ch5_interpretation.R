####################################################
# Applied Econometrics Studio
# Chapter 6 — Functional Forms
####################################################

graphics.off()

# --------------------------------------------------
# Safety check
# --------------------------------------------------

if (!file.exists("R/run_ch6_functional_forms.R")) {
  stop("Set working directory to AppliedEconometricsStudio")
}

# --------------------------------------------------
# Load dependencies
# --------------------------------------------------

source("R/simulate_ch4_data.R")
source("R/run_ch6_functional_forms.R")

# --------------------------------------------------
# Data & models
# --------------------------------------------------

df  <- simulate_ch4_data()
res <- run_ch6_functional_forms(df)

# --------------------------------------------------
# Layout
# --------------------------------------------------

layout(
  matrix(c(1,1, 2,3, 4,4), nrow = 3, byrow = TRUE),
  heights = c(1,6,2)
)

# --------------------------------------------------
# Header
# --------------------------------------------------

par(mar = c(1,1,2,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.5,0.6,"Applied Econometrics Studio",cex=1.6,font=2)
text(0.5,0.3,"Chapter 6 — Functional Forms",cex=1.1)

# --------------------------------------------------
# Controls (fake)
# --------------------------------------------------

par(mar = c(2,2,2,2))
plot.new()
rect(0,0,1,1,col="grey95",border=NA)
text(0.05,0.9,"Models compared",adj=0,font=2)

text(0.05,0.7,"• Linear",adj=0)
text(0.05,0.6,"• Log-linear (log y)",adj=0)
text(0.05,0.5,"• Quadratic",adj=0)

# --------------------------------------------------
# Visualization
# --------------------------------------------------

par(mar = c(4,4,2,1))

x_seq <- seq(min(df$x1), max(df$x1), length.out = 100)
x2_bar <- mean(df$x2)

# Predictions
y_lin <- coef(res$linear)["(Intercept)"] +
         coef(res$linear)["x1"] * x_seq +
         coef(res$linear)["x2"] * x2_bar

y_log <- exp(
  coef(res$loglin)["(Intercept)"] +
  coef(res$loglin)["x1"] * x_seq +
  coef(res$loglin)["x2"] * x2_bar
)

y_quad <- coef(res$quad)["(Intercept)"] +
          coef(res$quad)["x1"] * x_seq +
          coef(res$quad)["I(x1^2)"] * x_seq^2 +
          coef(res$quad)["x2"] * x2_bar

plot(
  df$x1, df$y,
  pch = 16,
  col = rgb(0.7,0.7,0.7,0.4),
  xlab = "x1",
  ylab = "y",
  main = "Same Data — Different Functional Forms"
)

lines(x_seq, y_lin,  col = "steelblue",  lwd = 2)
lines(x_seq, y_log,  col = "darkgreen",  lwd = 2)
lines(x_seq, y_quad, col = "firebrick",  lwd = 2)

legend(
  "topleft",
  legend = c("Linear", "Log-linear", "Quadratic"),
  col = c("steelblue", "darkgreen", "firebrick"),
  lwd = 2,
  bty = "n"
)

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)