####################################################
# Applied Econometrics Studio
# Chapter 4 — Multiple OLS (BASIC)
####################################################

graphics.off()

# --------------------------------------------------
# Auto-load Chapter 4 dependencies
# --------------------------------------------------

source("R/simulate_ch4_data.R")
source("R/run_multiple_ols.R")

# --------------------------------------------------
# Generate data and run model
# --------------------------------------------------

df  <- simulate_ch4_data()
res <- run_multiple_ols(df)

# --------------------------------------------------
# Pseudo-GUI layout
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
text(0.5,0.3,"Chapter 4 — Multiple Linear Regression",cex=1.1)

# --------------------------------------------------
# Controls (fake, auto)
# --------------------------------------------------

par(mar = c(2,2,2,2))
plot.new()
rect(0,0,1,1,col="grey95",border=NA)
text(0.05,0.9,"Controls",adj=0,font=2)

text(0.05,0.7,paste("Dependent variable (Y):", res$y),adj=0)
text(0.05,0.55,"Independent variables (X):",adj=0)
text(0.1,0.45,paste("-", paste(res$x, collapse = ", ")),adj=0)

rect(0.25,0.2,0.75,0.35,col="steelblue",border=NA)
text(0.5,0.275,"RUN MODEL",col="white",font=2)

# --------------------------------------------------
# Visualization — partial effect
# --------------------------------------------------

par(mar = c(4,4,2,1))
x1_res <- resid(lm(df$x1 ~ df$x2))
y_res  <- resid(lm(df$y  ~ df$x2))

plot(
  x1_res, y_res,
  pch = 19,
  col = "steelblue",
  xlab = "x1 (net of x2)",
  ylab = "y (net of x2)",
  main = "Partial Effect of x1"
)

abline(lm(y_res ~ x1_res), col = "firebrick", lwd = 2)

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interpretation,adj=0,cex=1.1)