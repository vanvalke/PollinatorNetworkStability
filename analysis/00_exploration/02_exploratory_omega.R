# analysis/00_exploration/02_exploratory_omega.R
# Exploratory analyses relating ω to network structure, species composition,
# and temporal patterns. Not hypothesis-testing — these are for pattern discovery.
#
# Plots:
#   01_omega_vs_richness.png        — ω ~ community size (S_eff), loess by guild
#   02_omega_vs_connectance.png     — ω ~ connectance, colored by year
#   03_omega_heatmap.png            — week × year heatmap of ω (combined)
#   04_omega_heatmap_site.png       — week × year heatmap per site
#   05_omega_vs_bee_richness.png    — ω ~ bee species richness
#   06_omega_species_profiles.png   — per-bee-species contribution to community-weeks
#   07_peak_week_trend.png          — combined + site peak week vs year scatter
#   08_omega_vs_evenness.png        — ω ~ bee evenness (J_bee)
#   09_omega_vs_total_interactions.png — ω ~ total interaction rate
#   10_site_year_heatmap.png        — mean ω heatmap: site × year
#
# Inputs:  data/processed/omega_combined.csv
#          data/processed/omega_site.csv
#          data/processed/interactions_long.csv
# Outputs: figs/00_exploration/*.png

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

omega_comb <- read_csv("data/processed/omega_combined.csv", show_col_types = FALSE)
omega_site <- read_csv("data/processed/omega_site.csv", show_col_types = FALSE) |>
  mutate(site = factor(site, levels = c("AW", "CC", "FP", "GS", "RH", "SP")))
interactions <- read_csv("data/processed/interactions_long.csv", show_col_types = FALSE)

dir.create("figs/00_exploration", showWarnings = FALSE, recursive = TRUE)

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

# ── Build joined descriptor table ─────────────────────────────────────────────

shannon <- function(x) {
  p <- x / sum(x)
  p <- p[p > 0]
  -sum(p * log(p))
}

desc_site <- interactions |>
  group_by(site.code, year, week) |>
  summarise(
    n_bee          = n_distinct(species),
    n_plant        = n_distinct(plant.species),
    total_rate     = sum(interaction_rate),
    n_pairs        = n(),
    H_bee          = shannon(tapply(interaction_rate, species, sum)),
    .groups = "drop"
  ) |>
  mutate(
    J_bee       = if_else(n_bee > 1, H_bee / log(n_bee), NA_real_),
    connectance = n_pairs / (n_bee * n_plant)
  ) |>
  rename(site = site.code)

# Site-level omega joined with descriptors
omega_site_desc <- omega_site |>
  left_join(desc_site, by = c("site", "year", "week"))

# Combined-level descriptors (pooled across sites per year-week)
desc_comb <- interactions |>
  group_by(year, week) |>
  summarise(
    n_bee       = n_distinct(species),
    n_plant     = n_distinct(plant.species),
    total_rate  = sum(interaction_rate),
    n_pairs     = n(),
    H_bee       = shannon(tapply(interaction_rate, species, sum)),
    .groups = "drop"
  ) |>
  mutate(
    J_bee       = if_else(n_bee > 1, H_bee / log(n_bee), NA_real_),
    connectance = n_pairs / (n_bee * n_plant)
  )

omega_comb_desc <- omega_comb |>
  left_join(
    select(desc_comb, year, week, total_rate, H_bee, J_bee, connectance),
    by = c("year", "week")
  )

# ── 1. ω vs community size (S_eff) ───────────────────────────────────────────
# Are large communities more or less feasible than small ones?
# Expect ω to rise with S_eff then flatten; early/late-season small communities
# should anchor the lower left.

p1 <- ggplot(omega_comb, aes(x = S_eff, y = omega)) +
  geom_point(aes(color = factor(year)), size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey80",
              linewidth = 1.0, span = 0.5) +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(
    title    = "Structural stability vs community size",
    subtitle = "S_eff = distinct species in network (bees + plants); all sites combined",
    x        = "Community size (S_eff)",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/00_exploration/02a_omega_vs_community_size.png",
       p1, width = 9, height = 5, dpi = 150)

# ── 2. ω vs connectance ───────────────────────────────────────────────────────
# More connected networks — does that help or hurt feasibility?

p2 <- ggplot(omega_comb_desc, aes(x = connectance, y = omega)) +
  geom_point(aes(color = factor(year), size = S_eff), alpha = 0.65) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey80",
              linewidth = 1.0, span = 0.6) +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_size_continuous(range = c(1, 4), name = "S_eff") +
  labs(
    title    = "Structural stability vs network connectance",
    subtitle = "Connectance = observed pairs / (n_bee × n_plant); all sites combined",
    x        = "Network connectance",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/00_exploration/02b_omega_vs_connectance.png",
       p2, width = 10, height = 5, dpi = 150)

# ── 3. Week × year heatmap (combined) ─────────────────────────────────────────
# The simplest full-data summary: every cell = one network.
# Reveals which year-weeks are missing and where the high-ω season core is.

heat_data <- omega_comb |>
  mutate(year = factor(year, levels = rev(2015:2023)))

p3 <- ggplot(heat_data, aes(x = week, y = year, fill = omega)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1,
                       limits = c(0.7, 1.0), oob = scales::squish,
                       name = omega_label) +
  scale_x_continuous(breaks = seq(2, 22, 2), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Structural stability heatmap — all-site combined networks",
    subtitle = "Each cell = one community-week; grey = no data",
    x        = "Week of season",
    y        = "Year"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position  = "right",
    panel.border     = element_rect(fill = NA, color = "grey60"),
    axis.line        = element_blank()
  )

ggsave("figs/00_exploration/02c_omega_heatmap_combined.png",
       p3, width = 11, height = 5, dpi = 150)

# ── 4. Week × year heatmap per site ──────────────────────────────────────────

heat_site <- omega_site |>
  mutate(year_f = factor(year, levels = rev(2015:2023)))

p4 <- ggplot(heat_site, aes(x = week, y = year_f, fill = omega)) +
  geom_tile(color = "white", linewidth = 0.25) +
  facet_wrap(~ site, nrow = 2) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1,
                       limits = c(0.65, 1.0), oob = scales::squish,
                       name = omega_label) +
  scale_x_continuous(breaks = seq(4, 22, 4), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Structural stability heatmap by site",
    subtitle = "Each cell = one site-community-week",
    x        = "Week of season",
    y        = "Year"
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position  = "right",
    panel.border     = element_rect(fill = NA, color = "grey60"),
    axis.line        = element_blank(),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave("figs/00_exploration/02d_omega_heatmap_site.png",
       p4, width = 14, height = 7, dpi = 150)

# ── 5. ω vs bee species richness ──────────────────────────────────────────────

p5 <- ggplot(omega_comb_desc, aes(x = n_bee, y = omega)) +
  geom_point(aes(color = factor(year)), size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey80",
              linewidth = 1.0, span = 0.6) +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_x_continuous(breaks = 1:9) +
  labs(
    title    = "Structural stability vs bee species richness",
    subtitle = "All sites combined",
    x        = "Bee species richness",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/00_exploration/02e_omega_vs_bee_richness.png",
       p5, width = 9, height = 5, dpi = 150)

# ── 6. Bee species presence across year-weeks ─────────────────────────────────
# Which species are present in most community-weeks? Shows who drives the networks.

bee_presence <- interactions |>
  distinct(year, week, species) |>
  count(species, name = "n_community_weeks") |>
  arrange(desc(n_community_weeks)) |>
  mutate(species = factor(species, levels = rev(species)))

p6 <- ggplot(bee_presence, aes(x = n_community_weeks, y = species)) +
  geom_col(fill = "#4393c3", alpha = 0.85) +
  geom_text(aes(label = n_community_weeks), hjust = -0.2, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Bee species breadth across community-weeks",
    subtitle = "Number of distinct year × week combinations each species appears in",
    x        = "Number of year-week combinations",
    y        = NULL
  ) +
  base_theme +
  theme(axis.text.y = element_text(face = "italic"))

ggsave("figs/00_exploration/02f_bee_species_presence.png",
       p6, width = 9, height = 5, dpi = 150)

# ── 7. Bee species occurrence grid ────────────────────────────────────────────
# Year × bee species grid — which species are present in each year?

bee_year <- interactions |>
  distinct(year, species) |>
  mutate(present = 1)

all_combos <- expand.grid(
  year    = 2015:2023,
  species = unique(interactions$species),
  stringsAsFactors = FALSE
)

bee_year_full <- left_join(all_combos, bee_year, by = c("year", "species")) |>
  mutate(
    present = replace_na(present, 0),
    species = factor(species, levels = bee_presence$species)  # order by breadth
  )

# Count years present per species
n_years_present <- bee_year_full |>
  group_by(species) |>
  summarise(n_years = sum(present), .groups = "drop")

bee_year_full <- left_join(bee_year_full, n_years_present, by = "species")

p7 <- ggplot(bee_year_full, aes(x = year, y = species, fill = factor(present))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(data = n_years_present,
            aes(x = 2024, y = species, label = paste0(n_years, "/9")),
            hjust = 0, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("0" = "grey90", "1" = "#2166ac"),
                    labels = c("Absent", "Present"),
                    name = "") +
  scale_x_continuous(breaks = 2015:2023, expand = expansion(mult = c(0, 0.07))) +
  labs(
    title    = "Bee species presence by year",
    subtitle = "Right: fraction of years present (out of 9)",
    x        = "Year",
    y        = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.y      = element_text(face = "italic"),
    panel.border     = element_rect(fill = NA, color = "grey60"),
    axis.line        = element_blank(),
    legend.position  = "top"
  )

ggsave("figs/00_exploration/02g_bee_species_by_year.png",
       p7, width = 11, height = 5, dpi = 150)

# ── 8. ω vs bee evenness (J_bee) ─────────────────────────────────────────────

p8 <- ggplot(omega_comb_desc |> filter(!is.na(J_bee)),
             aes(x = J_bee, y = omega)) +
  geom_point(aes(color = factor(year), size = S_eff), alpha = 0.65) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey80",
              linewidth = 1.0, span = 0.6) +
  scale_color_manual(values = year_colors, name = "Year") +
  scale_size_continuous(range = c(1, 4), name = "S_eff") +
  labs(
    title    = "Structural stability vs bee community evenness",
    subtitle = "Pielou's J (bee species weighted by interaction rate); all sites combined",
    x        = "Bee evenness (J_bee)",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/00_exploration/02h_omega_vs_evenness.png",
       p8, width = 9, height = 5, dpi = 150)

# ── 9. ω vs total interaction rate ───────────────────────────────────────────

p9 <- ggplot(omega_comb_desc, aes(x = total_rate, y = omega)) +
  geom_point(aes(color = factor(year)), size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "grey30", fill = "grey80",
              linewidth = 1.0, span = 0.6) +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(
    title    = "Structural stability vs total interaction rate",
    subtitle = "Total interaction rate = sum of visits/min across all bee×plant pairs",
    x        = "Total interaction rate (visits / min)",
    y        = omega_label
  ) +
  base_theme

ggsave("figs/00_exploration/02i_omega_vs_total_rate.png",
       p9, width = 9, height = 5, dpi = 150)

# ── 10. Site × year heatmap of mean ω ─────────────────────────────────────────
# Which sites are most stable? Which years were outliers?

site_yr_mean <- omega_site |>
  group_by(site, year) |>
  summarise(mean_omega = mean(omega, na.rm = TRUE), .groups = "drop") |>
  mutate(year = factor(year, levels = rev(2015:2023)))

p10 <- ggplot(site_yr_mean, aes(x = site, y = year, fill = mean_omega)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_omega, 2)), size = 3.2, color = "grey20") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1,
                       limits = c(0.83, 0.96), oob = scales::squish,
                       name = expression(italic(omega))) +
  labs(
    title    = "Mean seasonal structural stability by site and year",
    subtitle = "Each cell = mean ω across all weeks in that site-year",
    x        = "Site",
    y        = "Year"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border    = element_rect(fill = NA, color = "grey60"),
    axis.line       = element_blank(),
    legend.position = "right"
  )

ggsave("figs/00_exploration/02j_site_year_omega_heatmap.png",
       p10, width = 8, height = 6, dpi = 150)

message("Done. Saved 10 exploratory omega plots to figs/00_exploration/.")
