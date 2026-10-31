#!/usr/bin/env Rscript

library(jsonlite)
library(did)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No input file provided")
}

input <- fromJSON(args[1])
df    <- as.data.frame(input$data)

att <- att_gt(
  yname         = input$yname,
  tname         = input$tname,
  idname        = input$idname,
  gname         = input$gname,
  data          = df,
  panel         = TRUE,
  bstrap        = TRUE,
  cband         = TRUE,
  biters        = 199,
  pl            = FALSE,
  print_details = FALSE
)

agg_simple  <- aggte(att, type = "simple")
agg_dynamic <- aggte(att, type = "dynamic")

overall_att   <- agg_simple$overall.att
overall_se    <- agg_simple$overall.se
crit_overall  <- qnorm(0.975)
overall_lower <- overall_att - crit_overall * overall_se
overall_upper <- overall_att + crit_overall * overall_se
overall_pval  <- 2 * (1 - pnorm(abs(overall_att / overall_se)))

event_time <- agg_dynamic$egt
att_egt    <- agg_dynamic$att.egt
se_egt     <- agg_dynamic$se.egt
crit_sim   <- agg_dynamic$crit.val.egt
lower_ci   <- att_egt - crit_sim * se_egt
upper_ci   <- att_egt + crit_sim * se_egt

pre_idx <- which(event_time < 0)

if (length(pre_idx) > 0) {
  beta_pre <- att_egt[pre_idx]
  var_pre  <- (se_egt[pre_idx])^2
  valid    <- which(!is.na(beta_pre) & !is.na(var_pre) & var_pre > 0)

  if (length(valid) > 0) {
    beta_pre <- beta_pre[valid]
    var_pre  <- var_pre[valid]
    W_stat   <- sum((beta_pre^2) / var_pre)
    df_pre   <- length(beta_pre)
    p_pre    <- 1 - pchisq(W_stat, df_pre)
    pretrend_test <- list(
      statistic = as.numeric(W_stat),
      df        = df_pre,
      p_value   = as.numeric(p_pre)
    )
  } else {
    pretrend_test <- NULL
  }
} else {
  pretrend_test <- NULL
}

result <- list(
  inference = "bootstrap_simultaneous",
  overall = list(
    att      = as.numeric(overall_att),
    se       = as.numeric(overall_se),
    lower_ci = as.numeric(overall_lower),
    upper_ci = as.numeric(overall_upper),
    p_value  = as.numeric(overall_pval)
  ),
  dynamic = list(
    event_time = as.numeric(event_time),
    att_egt    = as.numeric(att_egt),
    se_egt     = as.numeric(se_egt),
    lower_ci   = as.numeric(lower_ci),
    upper_ci   = as.numeric(upper_ci),
    crit_value = as.numeric(crit_sim)
  ),
  pretrend_test = pretrend_test
)

cat(toJSON(result, auto_unbox = TRUE))
