#!/usr/bin/env Rscript

library(jsonlite)
library(did)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No input file provided")
}

input <- fromJSON(args[1])
df    <- as.data.frame(input$data)

run_placebo <- function(shift_val) {

  df_placebo <- df
  df_placebo[[input$gname]] <- df_placebo[[input$gname]] - shift_val

  att_p <- tryCatch({
    att_gt(
      yname         = input$yname,
      tname         = input$tname,
      idname        = input$idname,
      gname         = input$gname,
      data          = df_placebo,
      panel         = TRUE,
      bstrap        = FALSE,
      print_details = FALSE
    )
  }, error = function(e) return(NULL))

  if (is.null(att_p)) return(NULL)

  agg_p <- tryCatch({
    aggte(att_p, type = "simple")
  }, error = function(e) return(NULL))

  if (is.null(agg_p)) return(NULL)

  att_val <- agg_p$overall.att
  se_val  <- agg_p$overall.se

  if (is.na(att_val) || is.na(se_val)) return(NULL)

  p_val <- 2 * (1 - pnorm(abs(att_val / se_val)))

  return(list(
    shift   = shift_val,
    att     = as.numeric(att_val),
    se      = as.numeric(se_val),
    p_value = as.numeric(p_val)
  ))
}

placebo_1 <- run_placebo(1)
placebo_2 <- run_placebo(2)

result <- list(
  placebo_minus1 = placebo_1,
  placebo_minus2 = placebo_2
)

cat(toJSON(result, auto_unbox = TRUE))
