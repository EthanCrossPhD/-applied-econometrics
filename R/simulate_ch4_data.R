simulate_ch4_data <- function(n = 200) {
  set.seed(42)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y  <- 1 + 0.7 * x1 + 0.4 * x2 + rnorm(n)
  data.frame(y, x1, x2)
}