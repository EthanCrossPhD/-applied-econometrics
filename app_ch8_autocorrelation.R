####################################################
# Applied Econometrics Studio
# Chapter 8 — Autocorrelation
####################################################

graphics.off()
dev.new()

# --------------------------------------------------
# Safety check
# --------------------------------------------------

if (!file.exists("R/run_ch8_autocorrelation.R")) {
  stop("Set working directory to AppliedEconometricsStudio")
}

# --------------------------------------------------
# Load backend
# --------------------------------------------------

source("R/run_ch8_autocorrelation.R")

# --------------------------------------------------
# Generate data
# --------------------------------------------------

res <- run_ch8_autocorrelation()
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
text(0.5,0.3,"Chapter 8 — Autocorrelation",cex=1.1)

# --------------------------------------------------
# Controls (fake)
# --------------------------------------------------

par(mar = c(2,2,2,2))
plot.new()
rect(0,0,1,1,col="grey95",border=NA)
text(0.05,0.9,"Key idea",adj=0,font=2)

text(0.05,0.7,"• Errors move together over time",adj=0)
text(0.05,0.6,"• Patterns in residuals",adj=0)

# --------------------------------------------------
# Visualization
# --------------------------------------------------

par(mar = c(4,4,2,1))

plot(
  df$t, df$y,
  type = "l",
  col = "steelblue",
  lwd = 2,
  xlab = "Time",
  ylab = "y",
  main = "Time Series with Autocorrelated Errors"
)

abline(lm(y ~ t, data = df), col = "firebrick", lwd = 2)

# Residual pattern
resid_vals <- resid(res$model)

lines(
  df$t,
  fitted(res$model) + resid_vals,
  col = rgb(0.8,0.2,0.2,0.6),
  lwd = 2
)

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)