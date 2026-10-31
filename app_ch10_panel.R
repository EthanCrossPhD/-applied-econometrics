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

# --------------------------------------------------
# Interpretation
# --------------------------------------------------

par(mar = c(1,1,1,1))
plot.new()
rect(0,0,1,1,col="grey90",border=NA)
text(0.02,0.65,"Interpretation:",adj=0,font=2)
text(0.02,0.35,res$interp,adj=0,cex=1.1)