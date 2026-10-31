####################################################
# Chapter 10 — Difference-in-Differences (BASIC)
####################################################

run_ch10_did <- function(n = 200, tau = 2) {

  set.seed(1010)

  # ----------------------------------------------
  # 1. Data structure
  # ----------------------------------------------
  id <- rep(1:n, each = 2)
  time <- rep(c(0, 1), times = n)

  # Treatment group indicator
  treated <- rep(rbinom(n, 1, 0.5), each = 2)

  # Post-treatment period
  post <- as.numeric(time == 1)

  # ----------------------------------------------
  # 2. Structural components (DGP)
  # ----------------------------------------------
  alpha_i <- rep(rnorm(n, mean = 5, sd = 1), each = 2)  # unit fixed effects
  trend <- 1.5 * time                                  # common time trend
  eps <- rnorm(2 * n, sd = 1)                           # idiosyncratic noise

  # Treatment effect applies only to treated units in post period
  treatment_effect <- tau * treated * post

  # Outcome
  y <- alpha_i + trend + treatment_effect + eps

  data <- data.frame(
    id = id,
    time = time,
    treated = treated,
    post = post,
    y = y
  )

  # ----------------------------------------------
  # 3. Manual Difference-in-Differences
  # ----------------------------------------------
  mean_treated_post  <- mean(y[treated == 1 & post == 1])
  mean_treated_pre   <- mean(y[treated == 1 & post == 0])
  mean_control_post  <- mean(y[treated == 0 & post == 1])
  mean_control_pre   <- mean(y[treated == 0 & post == 0])

  did_manual <- (mean_treated_post - mean_treated_pre) -
                (mean_control_post - mean_control_pre)

  # ----------------------------------------------
  # 4. Regression DiD
  # ----------------------------------------------
  did_lm <- lm(y ~ treated + post + treated:post, data = data)
  did_reg <- coef(did_lm)["treated:post"]

  # ----------------------------------------------
  # 5. Interpretation text
  # ----------------------------------------------
  interpretation <- paste0(
    "Difference-in-Differences compares changes over time between treated ",
    "and control groups.\n\n",
    "The manual DiD estimate is ", round(did_manual, 3),
    ", while the regression-based estimate is ", round(did_reg, 3), ".\n\n",
    "Both estimates recover the true treatment effect τ = ", tau,
    " up to sampling variation."
  )

  # ----------------------------------------------
  # 6. Output
  # ----------------------------------------------
  list(
    data = data,
    did_manual = did_manual,
    did_regression = did_reg,
    true_tau = tau,
    interpretation = interpretation
  )
}

