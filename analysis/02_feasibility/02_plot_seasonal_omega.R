# analysis/02_feasibility/02_plot_seasonal_omega.R
# H1: Does the feasibility domain (ω) form a hump shape across the season?
#
# Plots:
#   01_omega_seasonal_combined.png   — one smoothed trajectory per year (combined data)
#   02_omega_seasonal_facet_year.png — one panel per year, hump shape visible
#   03_omega_seasonal_facet_site.png — site-level data, one panel per site × year ribbon
#   04_omega_peak_week_by_year.png   — when does the seasonal peak occur each year?
#   05_omega_seasonal_size_annotated.png — same as 01 but with S_eff shown as point size
#
# Inputs:  data/processed/omega_combined.csv
#          data/processed/omega_site.csv
# Outputs: figs/02_feasibility/*.png

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

omega_comb <- read_csv("data/processed/omega_combined.csv", show_col_types = FALSE) |>
  mutate(year = as.factor(year))
omega_site <- read_csv("data/processed/omega_site.csv", show_col_types = FALSE) |>
  mutate(year = as.factor(year),
         site = factor(site, levels = c("AW", "CC", "FP", "GS", "RH", "SP")))

dir.create("figs/02_feasibility", showWarnings = FALSE, recursive = TRUE)

# ── Shared aesthetics ─────────────────────────────────────────────────────────

year_colors <- setNames(
  colorRampPalette(c("#2166ac", "#4393c3", "#92c5de", "#4dac26", "#d6604d", "#b2182b"))(9),
  as.character(2015:2023)
)

base_theme <- theme_classic(base_size = 12) +
  theme(
    legend.position    = "right",
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10)
  )

omega_label <- expression(paste("Structural stability (", italic(omega), ")"))

# ── 1. All years on one panel — combined data ─────────────────────────────────
# Points + loess, colored by year. The hump pattern should be clear.

p1 <- ggplot(omega_comb, aes(x = week, y = omega, color = year, group = year)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, span = 0.55, na.rm = TRUE) +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(breaks = seq(2, 22, 2)) +
  scale_y_continuous(limits = c(0.65, 1.0), breaks = seq(0.7, 1.0, 0.05)) +
  labs(
    title    = "Seasonal trajectory of structural stability (H1)",
    subtitle = "All sites combined; each point is one community-week",
    x        = "Week of season",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/02_feasibility/01_omega_seasonal_combined.png",
       p1, width = 10, height = 5.5, dpi = 150)

# ── 2. One panel per year — hump shape clearly visible ───────────────────────

p2 <- ggplot(omega_comb, aes(x = week, y = omega)) +
  geom_point(aes(color = year), size = 1.6, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey70",
              linewidth = 0.9, span = 0.6, na.rm = TRUE) +
  facet_wrap(~ year, nrow = 3) +
  scale_color_manual(values = year_colors, guide = "none") +
  scale_x_continuous(breaks = seq(4, 22, 4)) +
  scale_y_continuous(limits = c(0.65, 1.0), breaks = c(0.7, 0.85, 1.0)) +
  labs(
    title    = "Seasonal hump in structural stability — by year (H1)",
    subtitle = "Loess ± 95% CI; all sites combined",
    x        = "Week of season",
    y        = omega_label
  ) +
  base_theme +
  theme(panel.spacing = unit(0.6, "lines"))

ggsave("figs/02_feasibility/02_omega_seasonal_facet_year.png",
       p2, width = 12, height = 8, dpi = 150)

# ── 3. Site-level facet — one panel per site, all years overlaid ──────────────

p3 <- ggplot(omega_site, aes(x = week, y = omega, color = year, group = year)) +
  geom_point(size = 1.1, alpha = 0.45) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.85, span = 0.6, na.rm = TRUE) +
  facet_wrap(~ site, nrow = 2, scales = "fixed") +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(breaks = seq(4, 22, 4)) +
  scale_y_continuous(limits = c(0.65, 1.0), breaks = c(0.7, 0.85, 1.0)) +
  labs(
    title    = "Seasonal structural stability by site (H1)",
    subtitle = "Individual site-level networks; loess smoothing",
    x        = "Week of season",
    y        = omega_label
  ) +
  base_theme +
  theme(panel.spacing = unit(0.6, "lines"))

ggsave("figs/02_feasibility/03_omega_seasonal_facet_site.png",
       p3, width = 14, height = 7, dpi = 150)

# ── 4. Peak week timing — when does the seasonal maximum occur each year? ──────
# Phenological shift in peak stability across years.

peak_combined <- omega_comb |>
  group_by(year) |>
  slice_max(omega, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(peak_week = week, peak_omega = omega)

peak_site <- omega_site |>
  group_by(site, year) |>
  slice_max(omega, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(peak_week = week, peak_omega = omega)

p4a <- ggplot(peak_site, aes(x = as.integer(as.character(year)), y = peak_week,
                              color = site, group = site)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_line(data = peak_combined,
            aes(x = as.integer(as.character(year)), y = peak_week),
            color = "black", linewidth = 1.4, linetype = "dashed",
            inherit.aes = FALSE) +
  geom_point(data = peak_combined,
             aes(x = as.integer(as.character(year)), y = peak_week),
             color = "black", size = 3.5, shape = 18,
             inherit.aes = FALSE) +
  scale_color_brewer(palette = "Dark2", name = "Site") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(breaks = seq(4, 20, 2)) +
  labs(
    title    = "Timing of seasonal stability peak across years",
    subtitle = "Dashed = combined network; colored = individual sites",
    x        = "Year",
    y        = "Week of peak ω"
  ) +
  base_theme

ggsave("figs/02_feasibility/04_omega_peak_week_timing.png",
       p4a, width = 10, height = 5, dpi = 150)

# ── 5. Size-annotated seasonal plot — S_eff as point size ─────────────────────
# Helps show that early/late season points are small networks (low S_eff)

p5 <- ggplot(omega_comb, aes(x = week, y = omega, color = year, size = S_eff)) +
  geom_point(alpha = 0.65) +
  scale_size_continuous(range = c(0.8, 5), name = "Community\nsize (S_eff)") +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(breaks = seq(2, 22, 2)) +
  scale_y_continuous(limits = c(0.65, 1.0), breaks = seq(0.7, 1.0, 0.05)) +
  labs(
    title    = "Structural stability by week, scaled by community size",
    subtitle = "Point size = number of interacting species (S_eff); all sites combined",
    x        = "Week of season",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/02_feasibility/05_omega_seasonal_size_annotated.png",
       p5, width = 11, height = 5.5, dpi = 150)

message("Done. Saved 5 seasonal omega plots to figs/02_feasibility/.")
