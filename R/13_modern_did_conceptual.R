
# =========================================================
# Chapter: Modern Difference-in-Differences
# Goal: Understanding modern DiD estimators
# =========================================================

source('R/00_global_setup.R')

cat('
--- Why TWFE fails with staggered treatment ---

Problem:
- Treatment occurs at different times.
- Treatment effects change over time.
- TWFE averages many DiD comparisons.
- Some comparisons use already-treated units as controls.

Result:
- Estimates can be biased.
- Weights can be negative.
')

cat('
--- Callaway–Sant’Anna (2021) ---

Core idea:
- Estimate treatment effects by:
  * treatment cohort (g)
  * calendar time (t)

ATT(g,t):
- Compare group treated at time g
- To units not yet treated at time t

Advantages:
- Clean control group
- Transparent identification
- Aggregation is explicit
')

cat('
--- Sun–Abraham (2021) ---

Core idea:
- Modify Event Study
- Use interactions that avoid forbidden comparisons

Key feature:
- No treated units used as controls
- Event-time coefficients are interpretable

Advantages:
- Compatible with TWFE framework
- Safe event-study plots
')

cat('
--- Practical Guidance ---

Use TWFE when:
- Treatment timing is the same for everyone
- Or effects are homogeneous

Use Modern DiD when:
- Treatment is staggered
- Effects evolve over time

Rule of thumb:
- If staggered adoption → TWFE is risky
- Prefer Callaway–Sant’Anna or Sun–Abraham
')

cat('
TAKEAWAY:
- Modern DiD fixes the core problem of TWFE.
- Identification must be explicit.
- Today, modern DiD is the applied standard.
')

