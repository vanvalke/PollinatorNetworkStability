# analysis/00_exploration/01_community_descriptors.R
# Exploratory plots of community structure across sites, weeks, and years.
#
# Metrics computed per (site, year, week):
#   n_bee            — bee species richness
#   n_plant          — plant species richness
#   n_obs            — total raw interaction count
#   H_bee            — Shannon diversity of bee species (weighted by n_interactions)
#   J_bee            — Pielou's evenness of bee species (H_bee / log(n_bee))
#   H_interaction    — Shannon diversity of bee×plant interaction types
#   connectance      — fraction of possible bee×plant pairs observed
#
# Two versions of each plot:
#   figs/00_exploration/          — points + loess per year, all sites pooled
#   figs/00_exploration/faceted/  — same but facet_wrap(~ site), fixed axes
#
# Inputs:  data/processed/interactions_long.csv
# Outputs: figs/00_exploration/*.png
#          figs/00_exploration/faceted/*.png

library(readr)
library(dplyr)
library(ggplot2)

interactions <- read_csv("data/processed/interactions_long.csv", show_col_types = FALSE)

# ── Compute per-(site, year, week) descriptors ────────────────────────────────

shannon <- function(x) {
  p <- x / sum(x)
  p <- p[p > 0]
  -sum(p * log(p))
}

descriptors <- interactions |>
  group_by(site.code, year, week) |>
  summarise(
    n_bee         = n_distinct(species),
    n_plant       = n_distinct(plant.species),
    n_obs         = sum(n_interactions),
    H_bee         = shannon(tapply(n_interactions, species, sum)),
    H_interaction = shannon(n_interactions),
    n_pairs       = n(),
    .groups = "drop"
  ) |>
  mutate(
    J_bee       = if_else(n_bee > 1, H_bee / log(n_bee), NA_real_),
    connectance = n_pairs / (n_bee * n_plant),
    year        = as.factor(year)
  )

# ── Shared aesthetics ─────────────────────────────────────────────────────────

year_colors <- setNames(
  colorRampPalette(c("#2166ac", "#4dac26", "#d6604d"))(9),
  as.character(2015:2023)
)

jitter_pos <- position_jitter(width = 0.25, height = 0, seed = 42)

base_theme <- theme_classic(base_size = 11) +
  theme(
    legend.position    = "right",
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold")
  )

# ── Plot builders ─────────────────────────────────────────────────────────────

# Main plot: all sites pooled, one loess curve per year
make_main <- function(data, y_var, y_lab, title, ylims = NULL) {
  p <- ggplot(data, aes(x = week, y = .data[[y_var]], color = year, group = year)) +
    geom_point(position = jitter_pos, size = 0.9, alpha = 0.4, na.rm = TRUE) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.9,
                alpha = 0.15, na.rm = TRUE, span = 0.5) +
    scale_color_manual(values = year_colors, name = "Year") +
    scale_x_continuous(breaks = seq(2, 22, 2)) +
    labs(title = title, x = "Week of season", y = y_lab) +
    base_theme
  if (!is.null(ylims)) p <- p + coord_cartesian(ylim = ylims)
  p
}

# Faceted plot: same data, split by site, fixed axes
make_faceted <- function(data, y_var, y_lab, title, ylims = NULL) {
  p <- ggplot(data, aes(x = week, y = .data[[y_var]], color = year, group = year)) +
    geom_point(position = jitter_pos, size = 0.8, alpha = 0.45, na.rm = TRUE) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.85,
                alpha = 0.15, na.rm = TRUE, span = 0.6) +
    facet_wrap(~ site.code, nrow = 2, scales = "fixed") +
    scale_color_manual(values = year_colors, name = "Year") +
    scale_x_continuous(breaks = seq(4, 22, 4)) +
    labs(title = title, x = "Week of season", y = y_lab) +
    base_theme
  if (!is.null(ylims)) p <- p + coord_cartesian(ylim = ylims)
  p
}

save_both <- function(p_main, p_faceted, filename, w_main = 9, w_fac = 13) {
  ggsave(file.path("figs/00_exploration", filename),
         p_main, width = w_main, height = 5, dpi = 150)
  ggsave(file.path("figs/00_exploration/faceted", filename),
         p_faceted, width = w_fac, height = 7, dpi = 150)
}

dir.create("figs/00_exploration/faceted", showWarnings = FALSE, recursive = TRUE)

# ── 1. Bee species richness ───────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "n_bee", "Bee species richness (S)", "Bee species richness"),
  make_faceted( descriptors, "n_bee", "Bee species richness (S)", "Bee species richness by site"),
  "01_bee_richness.png"
)

# ── 2. Plant species richness ─────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "n_plant", "Plant species richness (S)", "Plant species richness"),
  make_faceted( descriptors, "n_plant", "Plant species richness (S)", "Plant species richness by site"),
  "02_plant_richness.png"
)

# ── 3. Total interactions ─────────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "n_obs", "Total interactions (raw count)", "Total observed interactions"),
  make_faceted( descriptors, "n_obs", "Total interactions (raw count)", "Total observed interactions by site"),
  "03_total_interactions.png"
)

# ── 4. Bee Shannon diversity ──────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "H_bee", "Bee Shannon diversity (H)", "Bee Shannon diversity"),
  make_faceted( descriptors, "H_bee", "Bee Shannon diversity (H)", "Bee Shannon diversity by site"),
  "04_bee_shannon.png"
)

# ── 5. Bee Pielou's evenness ──────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "J_bee", "Bee Pielou's evenness (J)", "Bee evenness", ylims = c(0, 1)),
  make_faceted( descriptors, "J_bee", "Bee Pielou's evenness (J)", "Bee evenness by site", ylims = c(0, 1)),
  "05_bee_evenness.png"
)

# ── 6. Interaction diversity ──────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "H_interaction", "Interaction Shannon diversity (H)", "Bee–plant interaction diversity"),
  make_faceted( descriptors, "H_interaction", "Interaction Shannon diversity (H)", "Bee–plant interaction diversity by site"),
  "06_interaction_diversity.png"
)

# ── 7. Connectance ────────────────────────────────────────────────────────────

save_both(
  make_main(    descriptors, "connectance", "Connectance (observed / possible pairs)", "Network connectance", ylims = c(0, 1)),
  make_faceted( descriptors, "connectance", "Connectance (observed / possible pairs)", "Network connectance by site", ylims = c(0, 1)),
  "07_connectance.png"
)

message("Done. Saved 7 main + 7 faceted plots.")
