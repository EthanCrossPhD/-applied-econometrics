options(digits = 4, scipen = 999)

library(conflicted)
library(tidyverse)
library(broom)
library(scales)
library(patchwork)

conflicts_prefer(dplyr::filter, dplyr::lag)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray30"),
      panel.grid.minor = element_blank()
    )
)

COL_TREATED <- "#1f77b4"
COL_CONTROL <- "#7f7f7f"
COL_ERROR   <- "#d62728"

