# R/16_did_comparison.R
# DiD Estimator Comparison: TWFE vs Callaway-Sant'Anna vs Sun-Abraham
# Faza 1.2 — researcher track core

suppressPackageStartupMessages({
  library(tidyverse)
  library(did)
  library(fixest)
  library(patchwork)
})

cat("--- DiD Comparison: TWFE vs CS vs SA ---\n\n")

# ── 0. DGP ───────────────────────────────────────────────────────────────────
set.seed(2024)
n_units <- 100; n_periods <- 8

df <- expand_grid(id = 1:n_units, time = 1:n_periods) %>%
  mutate(
    g          = case_when(id <= 33 ~ 3, id <= 66 ~ 5, id <= 90 ~ 7, TRUE ~ 0),
    event_time = if_else(g == 0, NA_real_, time - g),
    treat      = if_else(!is.na(event_time) & event_time >= 0, 1L, 0L),
    tau        = if_else(treat == 1L, 0.5 * event_time, 0),
    y          = 5 + 0.3 * time + tau + rnorm(n()),
    g_cohort   = if_else(g == 0, Inf, g)
  )

# ── 1. TWFE single coefficient ───────────────────────────────────────────────
twfe_fit <- feols(y ~ treat | id + time, data = df, cluster = ~id)
twfe_est <- coef(twfe_fit)[["treat"]]
twfe_se  <- se(twfe_fit)[["treat"]]
cat(sprintf("TWFE : ATT = %.3f  SE = %.3f\n", twfe_est, twfe_se))

# TWFE Event Study — bin tails, never-treated dobiva rel=-9999 (ispada iz faktora)
df_es <- df %>%
  mutate(
    rel_bin = pmax(pmin(if_else(g == 0L, -9999L, time - g), 5L), -5L),
    rel_f   = relevel(factor(rel_bin), ref = "-1")
  )

twfe_es <- feols(y ~ rel_f | id + time, data = df_es, cluster = ~id)

twfe_es_df <- tibble(
  term     = names(coef(twfe_es)),
  estimate = coef(twfe_es),
  se       = se(twfe_es)
) %>%
  filter(str_detect(term, "^rel_f-?[0-9]")) %>%
  mutate(
    event_time = as.integer(str_remove(term, "rel_f")),
    conf.low   = estimate - 1.96 * se,
    conf.high  = estimate + 1.96 * se,
    estimator  = "TWFE"
  ) %>%
  select(event_time, estimate, conf.low, conf.high, estimator) %>%
  add_row(event_time = -1L, estimate = 0, conf.low = 0,
          conf.high = 0, estimator = "TWFE") %>%
  arrange(event_time)

# ── 2. Callaway-Sant'Anna ─────────────────────────────────────────────────────
cs_gt <- att_gt(
  yname = "y", tname = "time", idname = "id", gname = "g",
  data  = df, panel = TRUE,
  control_group = "nevertreated",
  print_details = FALSE
)

cs_overall <- aggte(cs_gt, type = "simple")
cs_dyn     <- aggte(cs_gt, type = "dynamic")
cs_att     <- cs_overall$overall.att
cs_se      <- cs_overall$overall.se

cat(sprintf("CS   : ATT = %.3f  SE = %.3f\n", cs_att, cs_se))

cs_es_df <- tibble(
  event_time = cs_dyn$egt,
  estimate   = cs_dyn$att.egt,
  conf.low   = cs_dyn$att.egt - 1.96 * cs_dyn$se.egt,
  conf.high  = cs_dyn$att.egt + 1.96 * cs_dyn$se.egt,
  estimator  = "CS"
)

# ── 3. Sun-Abraham ────────────────────────────────────────────────────────────
sa_fit <- feols(
  y ~ sunab(g_cohort, time) | id + time,
  data = df, cluster = ~id
)

# Koeficijenti su po cohort × event-time; agregiramo na event-time (equal weights)
sa_raw <- tibble(
  term     = names(coef(sa_fit)),
  estimate = coef(sa_fit),
  se       = se(sa_fit)
) %>%
  filter(str_detect(term, "time::")) %>%
  mutate(event_time = as.integer(str_extract(term, "(?<=time::)-?\\d+"))) %>%
  group_by(event_time) %>%
  summarise(
    estimate = mean(estimate),
    se       = sqrt(mean(se^2)),
    .groups  = "drop"
  ) %>%
  mutate(
    conf.low  = estimate - 1.96 * se,
    conf.high = estimate + 1.96 * se,
    estimator = "SA"
  )

sa_es_df <- sa_raw %>%
  select(event_time, estimate, conf.low, conf.high, estimator) %>%
  add_row(event_time = -1L, estimate = 0, conf.low = 0,
          conf.high = 0, estimator = "SA") %>%
  arrange(event_time)

sa_att <- sa_raw %>% filter(event_time >= 0) %>% pull(estimate) %>% mean()
cat(sprintf("SA   : ATT = %.3f  (avg post event-times)\n\n", sa_att))

# ── 4. Summary table ─────────────────────────────────────────────────────────
cat("=== Summary ===\n")
cat(sprintf("  %-6s : %.3f\n", "TWFE", twfe_est))
cat(sprintf("  %-6s : %.3f\n", "CS",   cs_att))
cat(sprintf("  %-6s : %.3f\n\n", "SA",   sa_att))

# ── 5. Overall ATT plot ───────────────────────────────────────────────────────
att_df <- tibble(
  estimator = factor(c("TWFE", "CS", "SA"), levels = c("TWFE", "CS", "SA")),
  estimate  = c(twfe_est, cs_att, sa_att),
  conf.low  = c(twfe_est - 1.96 * twfe_se, cs_att - 1.96 * cs_se, NA_real_),
  conf.high = c(twfe_est + 1.96 * twfe_se, cs_att + 1.96 * cs_se, NA_real_)
)

colours <- c(TWFE = "#E63946", CS = "#2A9D8F", SA = "#457B9D")

p_att <- ggplot(att_df, aes(x = estimator, y = estimate, colour = estimator)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 5) +
  scale_colour_manual(values = colours) +
  labs(title    = "Overall ATT by Estimator",
       subtitle = "TWFE downward biased; CS & SA recover true weighted average",
       x = NULL, y = "ATT estimate") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ── 6. Event Study plot ───────────────────────────────────────────────────────
es_all <- bind_rows(twfe_es_df, cs_es_df, sa_es_df) %>%
  filter(event_time >= -5, event_time <= 5) %>%
  mutate(estimator = factor(estimator, levels = c("TWFE", "CS", "SA")))

p_es <- ggplot(es_all, aes(x = event_time, y = estimate,
                            colour = estimator, fill = estimator)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
  scale_colour_manual(values = colours) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks = -5:5) +
  labs(title    = "Event Study: TWFE vs CS vs SA",
       subtitle = "TWFE: spurious pre-trends  |  CS & SA: flat pre-trends",
       x = "Event time (relative to treatment)", y = "Estimate",
       colour = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ── 7. Patchwork & save ───────────────────────────────────────────────────────
combined <- p_att / p_es +
  plot_annotation(
    title    = "DiD Estimator Comparison",
    subtitle = "Staggered adoption  ·  τ = 0.5 × event_time  ·  N=100, T=8",
    caption  = "Groups: {3,5,7}  |  Control: never-treated  |  CS=Callaway-Sant'Anna (2021), SA=Sun-Abraham (2021)"
  )

dir.create("figures", showWarnings = FALSE)
ggsave("figures/16_did_comparison.png", combined, width = 10, height = 11, dpi = 150)
cat("Plot saved → figures/16_did_comparison.png\n\n")

cat("TAKEAWAY:\n")
cat("- TWFE: negative weights compress dynamic effects into one biased estimate.\n")
cat("- CS (doubly-robust): ATT(g,t) transparently identified, never-treated control.\n")
cat("- SA (cohort x time interactions): equivalent to CS under same assumptions.\n")
cat("- CS ≈ SA >>> TWFE under heterogeneous dynamic treatment effects.\n")
cat("- Pre-trend test: do not trust the TWFE version; CS/SA are flat as expected.\n")