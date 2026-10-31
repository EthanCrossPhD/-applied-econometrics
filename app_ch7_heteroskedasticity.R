####################################################
# Applied Econometrics Studio
# Chapter 7 — Heteroskedasticity
####################################################

graphics.off()
dev.new()

# --------------------------------------------------
# Safety check
# --------------------------------------------------

if (!file.exists("R/run_ch7_heteroskedasticity.R")) {
  stop("Set working directory to AppliedEconometricsStudio")
}

# --------------------------------------------------
# Load backend
# --------------------------------------------------

source("R/run_ch7_heteroskedasticity.R")

# --------------------------------------------------
# Generate data
# --------------------------------------------------

res <- run_ch7_heteroskedasticity()
df  <- res$data

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
text(0.5,0.3,"Chapter 7 — Heteroskedasticity",cex=1.1)

# --------------------------------------------------
# Controls (fake)
# --------------------------------------------------

par(mar = c(2,2,2,2))
plot.new()
rect(0,0,1,1,col="grey95",border=NA)
text(0.05,0.9,"What to look at",adj=0,font=2)

text(0.05,0.7,"• Spread of residuals increases with x",adj=0)
text(0.05,0.6,"• Funnel-shaped pattern",adj=0)

# --------------------------------------------------
# Visualization
# --------------------------------------------------

par(mar = c(4,4,2,1))

plot(
  df$x, df$y,
  pch = 16,
  col = rgb(0.2,0.4,0.7,0.5),
  xlab = "x",
  ylab = "y",
  main = "Heteroskedasticity: Increasing Variance"
)

abline(lm(y ~ x, data = df), col = "firebrick", lwd = 2)

# Residuals plot
resid_vals <- resid(res$model)

points(
  df$x,
  fitted(res$model) + resid_vals,
  pch = 16,
  col = rgb(0.8,0.2,0.2,0.4)
)

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)