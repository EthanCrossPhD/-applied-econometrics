####################################################
# Applied Econometrics Studio
# Chapter 10 — Panel Data (Intro)
####################################################

graphics.off()
dev.new()

# --------------------------------------------------
# Safety check
# --------------------------------------------------

if (!file.exists("R/run_ch10_panel.R")) {
  stop("Set working directory to AppliedEconometricsStudio")
}

# --------------------------------------------------
# Load backend
# --------------------------------------------------

source("R/run_ch10_panel.R")

# --------------------------------------------------
# Generate data
# --------------------------------------------------

res <- run_ch10_panel()
df  <- res$data
library(fixest)

# Pooled OLS
m_pool <- feols(y ~ x, data = df)

# Fixed Effects (individual)
m_fe <- feols(y ~ x | id, data = df)

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
text(0.5,0.3,"Chapter 10 — Panel Data (Intro)",cex=1.1)

# --------------------------------------------------
# Controls (fake)
# --------------------------------------------------

par(mar = c(2,2,2,2))
plot.new()
rect(0,0,1,1,col="grey95",border=NA)
text(0.05,0.9,"Key ideas",adj=0,font=2)

text(0.05,0.7,"• Individuals observed over time",adj=0)
text(0.05,0.6,"• Unobserved heterogeneity",adj=0)
text(0.05,0.5,"• Fixed effects control for it",adj=0)

# --------------------------------------------------
# Visualization
# --------------------------------------------------

par(mar = c(4,4,2,1))

# Plot few individuals
ids_to_plot <- unique(df$id)[1:6]
colors <- rainbow(length(ids_to_plot))

plot(
  df$time[df$id == ids_to_plot[1]],
  df$y[df$id == ids_to_plot[1]],
  type = "l",
  col = colors[1],
  lwd = 2,
  xlab = "Time",
  ylab = "y",
  main = "Panel Structure: Same Individuals Over Time",
  ylim = range(df$y)
)

for (i in seq_along(ids_to_plot)[-1]) {
  lines(
    df$time[df$id == ids_to_plot[i]],
    df$y[df$id == ids_to_plot[i]],
    col = colors[i],
    lwd = 2
  )
}

legend(
  "topleft",
  legend = paste("Individual", ids_to_plot),
  col = colors,
  lwd = 2,
  bty = "n"
)


# FE vs pooled slopes
beta_pool <- coef(m_pool)["x"]
beta_fe   <- coef(m_fe)["x"]

png("figures/fe_vs_pooled.png", width = 900, height = 600)

barplot(
  c(beta_pool, beta_fe),
  names.arg = c("Pooled OLS", "Fixed Effects"),
  col = c("grey70", "steelblue"),
  main = "Effect of x: Pooled vs Fixed Effects",
  ylab = "Coefficient"
)

abline(h = 0, lty = 2)

dev.off()

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)