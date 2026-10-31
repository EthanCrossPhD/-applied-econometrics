####################################################
# Applied Econometrics Studio
# Chapter 8 — Instrumental Variables (Application)
####################################################

graphics.off()
dev.new()

# --------------------------------------------------
# Safety check
# --------------------------------------------------

if (!file.exists("R/run_ch9_iv.R")) {
  stop("Set working directory to AppliedEconometricsStudio")
}

# --------------------------------------------------
# Load backend
# --------------------------------------------------

source("R/run_ch9_iv.R")

# --------------------------------------------------
# Generate data
# --------------------------------------------------

res <- run_ch9_iv()
df  <- res$data

# --------------------------------------------------
# True parameter (known by design)
# --------------------------------------------------

beta_true <- 1

# --------------------------------------------------
# Models
# --------------------------------------------------

# OLS (endogenous)
m_ols <- lm(y ~ x, data = df)

# -------------------------------
# IV via manual 2SLS
# -------------------------------

# First stage
m_fs <- lm(x ~ z, data = df)
x_hat <- fitted(m_fs)

# Second stage
m_iv <- lm(y ~ x_hat, data = df)

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
text(0.5,0.3,"Chapter 8 — Instrumental Variables",cex=1.1)

# --------------------------------------------------
# Visualization 1: First stage
# --------------------------------------------------

par(mar = c(4,4,2,1))

plot(
  df$z,
  df$x,
  pch = 16,
  col = rgb(0.2,0.2,0.7,0.5),
  xlab = "Instrument (z)",
  ylab = "Endogenous regressor (x)",
  main = "First Stage: Instrument Relevance"
)

abline(m_fs, col = "darkred", lwd = 2)

# --------------------------------------------------
# Visualization 2: OLS vs IV
# --------------------------------------------------

beta_ols <- coef(m_ols)["x"]
beta_iv  <- coef(m_iv)["x_hat"]

barplot(
  c(beta_ols, beta_iv, beta_true),
  names.arg = c("OLS", "IV", "True"),
  col = c("grey70", "steelblue", "darkgreen"),
  main = "Endogeneity Bias and IV Correction",
  ylab = "Coefficient"
)

abline(h = beta_true, lty = 2)

# --------------------------------------------------
# Save figure
# --------------------------------------------------

png("figures/iv_vs_ols.png", width = 900, height = 600)

barplot(
  c(beta_ols, beta_iv, beta_true),
  names.arg = c("OLS", "IV", "True"),
  col = c("grey70", "steelblue", "darkgreen"),
  main = "Endogeneity Bias and IV Correction",
  ylab = "Coefficient"
)

abline(h = beta_true, lty = 2)

dev.off()

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)