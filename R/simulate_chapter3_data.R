simulate_chapter3_data <- function(n = 100, seed = 123) {
  set.seed(seed)

  data.frame(
    y = rnorm(n, mean = 10, sd = 2),
    x = rnorm(n, mean = 5, sd = 1)
  )
}
