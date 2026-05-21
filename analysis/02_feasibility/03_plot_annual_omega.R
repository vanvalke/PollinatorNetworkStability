# analysis/02_feasibility/03_plot_annual_omega.R
# H2: Does the feasibility domain show resilience across years, with a possible
#     directional trend over 2015–2023?
#
# Plots:
#   06_omega_annual_boxplot.png       — distribution of ω by year (combined data)
#   07_omega_annual_peak_trend.png    — peak ω per year + linear trend
#   08_omega_midsession_trend.png     — ω at mid-season (weeks 8–14) across years
#   09_omega_annual_boxplot_site.png  — site-level distributions by year
#   10_omega_year_trajectories.png    — site-level mean ω per year, connected lines
#
# Inputs:  data/processed/omega_combined.csv
#          data/processed/omega_site.csv
# Outputs: figs/02_feasibility/*.png

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

omega_comb <- read_csv("data/processed/omega_combined.csv", show_col_types = FALSE)
omega_site <- read_csv("data/processed/omega_site.csv", show_col_types = FALSE) |>
  mutate(site = factor(site, levels = c("AW", "CC", "FP", "GS", "RH", "SP")))

dir.create("figs/02_feasibility", showWarnings = FALSE, recursive = TRUE)

# ── Shared aesthetics ─────────────────────────────────────────────────────────

year_colors <- setNames(
  colorRampPalette(c("#2166ac", "#4393c3", "#92c5de", "#4dac26", "#d6604d", "#b2182b"))(9),
  as.character(2015:2023)
)

base_theme <- theme_classic(base_size = 12) +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10)
  )

omega_label <- expression(paste("Structural stability (", italic(omega), ")"))

# ── 6. Annual boxplot — combined networks ─────────────────────────────────────
# Distribution of ω across all community-weeks within each year.
# If H2 holds (resilience), boxes should stay similar year to year.

p6 <- ggplot(omega_comb, aes(x = factor(year), y = omega, fill = factor(year))) +
  geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.5, linewidth = 0.5) +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = "grey30") +
  scale_fill_manual(values = year_colors, guide = "none") +
  scale_y_continuous(limits = c(0.65, 1.0), breaks = seq(0.7, 1.0, 0.05)) +
  labs(
    title    = "Annual distribution of structural stability (H2)",
    subtitle = "All sites combined; each point = one community-week",
    x        = "Year",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/02_feasibility/06_omega_annual_boxplot.png",
       p6, width = 10, height = 5, dpi = 150)

# ── 7. Peak ω per year + linear trend ─────────────────────────────────────────
# Tests whether the seasonal maximum is shifting directionally.

peak_comb <- omega_comb |>
  group_by(year) |>
  summarise(
    peak_omega  = max(omega, na.rm = TRUE),
    mean_omega  = mean(omega, na.rm = TRUE),
    median_omega = median(omega, na.rm = TRUE),
    .groups = "drop"
  )

lm_peak  <- lm(peak_omega   ~ year, data = peak_comb)
lm_mean  <- lm(mean_omega   ~ year, data = peak_comb)

p7 <- ggplot(peak_comb, aes(x = year)) +
  geom_line(aes(y = peak_omega,   color = "Peak ω"),   linewidth = 1.0) +
  geom_line(aes(y = mean_omega,   color = "Mean ω"),   linewidth = 1.0, linetype = "dashed") +
  geom_line(aes(y = median_omega, color = "Median ω"), linewidth = 1.0, linetype = "dotted") +
  geom_point(aes(y = peak_omega,   color = "Peak ω"),   size = 3.0) +
  geom_point(aes(y = mean_omega,   color = "Mean ω"),   size = 2.5) +
  geom_point(aes(y = median_omega, color = "Median ω"), size = 2.5) +
  geom_smooth(aes(y = peak_omega), method = "lm", se = TRUE,
              color = "grey40", fill = "grey80", linewidth = 0.7,
              linetype = "solid") +
  scale_color_manual(values = c("Peak ω" = "#d6604d", "Mean ω" = "#2166ac",
                                "Median ω" = "#4dac26"),
                     name = "") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(limits = c(0.88, 1.0), breaks = seq(0.88, 1.0, 0.02)) +
  labs(
    title    = "Annual structural stability — peak, mean, and median (H2)",
    subtitle = paste0("Peak trend: slope = ",
                      round(coef(lm_peak)[2], 5), " per year; ",
                      "Mean trend: slope = ", round(coef(lm_mean)[2], 5)),
    x        = "Year",
    y        = omega_label
  ) +
  base_theme +
  theme(legend.position = "bottom")

ggsave("figs/02_feasibility/07_omega_annual_peak_trend.png",
       p7, width = 10, height = 5.5, dpi = 150)

# ── 8. Mid-season ω across years ──────────────────────────────────────────────
# Focus on the most robust part of the season (weeks 8–14 cover most peak weeks).
# Year-to-year variation here is the most ecologically meaningful signal for H2.

midseason <- omega_comb |>
  filter(week >= 8, week <= 14) |>
  group_by(year) |>
  summarise(
    mean_mid   = mean(omega, na.rm = TRUE),
    median_mid = median(omega, na.rm = TRUE),
    sd_mid     = sd(omega, na.rm = TRUE),
    n          = n(),
    .groups = "drop"
  ) |>
  mutate(se_mid = sd_mid / sqrt(n))

lm_mid <- lm(mean_mid ~ year, data = midseason)

p8 <- ggplot(midseason, aes(x = year, y = mean_mid)) +
  geom_ribbon(aes(ymin = mean_mid - se_mid, ymax = mean_mid + se_mid),
              fill = "#4393c3", alpha = 0.25) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_point(aes(fill = factor(year)), color = "white", size = 4, shape = 21,
             stroke = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", fill = "grey80",
              linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = year_colors, guide = "none") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(breaks = seq(0.92, 1.0, 0.01)) +
  labs(
    title    = "Mid-season structural stability across years (H2 — weeks 8–14)",
    subtitle = paste0("Mean ± SE; linear trend slope = ",
                      round(coef(lm_mid)[2], 5), " per year"),
    x        = "Year",
    y        = expression(paste("Mean mid-season ", italic(omega)))
  ) +
  base_theme

ggsave("figs/02_feasibility/08_omega_midsession_trend.png",
       p8, width = 10, height = 5, dpi = 150)

# ── 9. Site-level annual boxplots ─────────────────────────────────────────────
# Each panel = one site; boxes show intra-annual ω distribution.

p9 <- ggplot(omega_site, aes(x = factor(year), y = omega, fill = factor(year))) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5, linewidth = 0.4) +
  facet_wrap(~ site, nrow = 2) +
  scale_fill_manual(values = year_colors, guide = "none") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4)) +  # short year labels
  scale_y_continuous(limits = c(0.65, 1.0), breaks = c(0.7, 0.85, 1.0)) +
  labs(
    title    = "Annual structural stability distributions by site (H2)",
    subtitle = "Each box = all weeks within a year × site",
    x        = "Year",
    y        = omega_label
  ) +
  base_theme +
  theme(panel.spacing = unit(0.6, "lines"))

ggsave("figs/02_feasibility/09_omega_annual_boxplot_site.png",
       p9, width = 14, height = 7, dpi = 150)

# ── 10. Site-level mean ω trajectories across years ───────────────────────────
# All sites on one panel, connected year-to-year — do sites track each other?

site_annual <- omega_site |>
  group_by(site, year) |>
  summarise(mean_omega = mean(omega, na.rm = TRUE), .groups = "drop")

# Also add combined network mean for reference
comb_annual <- omega_comb |>
  group_by(year) |>
  summarise(mean_omega = mean(omega, na.rm = TRUE), .groups = "drop") |>
  mutate(site = "ALL")

p10 <- ggplot(site_annual, aes(x = year, y = mean_omega, color = site, group = site)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_line(data = comb_annual, linewidth = 1.5, color = "black",
            linetype = "dashed", alpha = 0.7) +
  geom_point(data = comb_annual, size = 3.5, color = "black",
             shape = 18) +
  scale_color_brewer(palette = "Dark2", name = "Site") +
  scale_x_continuous(breaks = 2015:2023) +
  labs(
    title    = "Year-to-year trajectories of mean structural stability by site (H2)",
    subtitle = "Dashed = all-site combined network; colored = individual sites",
    x        = "Year",
    y        = expression(paste("Mean seasonal ", italic(omega)))
  ) +
  base_theme +
  theme(legend.position = "right")

ggsave("figs/02_feasibility/10_omega_year_trajectories.png",
       p10, width = 11, height = 5.5, dpi = 150)

message("Done. Saved 5 annual omega plots (06–10) to figs/02_feasibility/.")
