# analysis/01_data_prep/01_build_weekly_matrices.R
# Inputs:  data/bee_flw_data.csv
# Outputs: data/processed/interactions_long.csv
#          data/processed/weekly_matrices.rds       (per site × year × week)
#          data/processed/weekly_matrices_combined.rds  (per year × week, all sites pooled)

library(readr)
library(dplyr)
library(tidyr)

raw <- read_csv("data/bee_flw_data.csv", show_col_types = FALSE) |>
  mutate(week = as.integer(floor(week)))

# Copper.Creek 2022 week 11 has 35 observations each on a different date spanning
# ~6 calendar weeks — a clear week-labeling error in the source data. Exclude it.
raw <- raw |>
  anti_join(
    raw |>
      group_by(site.code, year, week) |>
      summarise(n_dates = n_distinct(date), .groups = "drop") |>
      filter(n_dates > 2),
    by = c("site.code", "year", "week")
  )

# Effort: total observation minutes per (site, year, week) from ALL behaviors so
# that bouts with no foraging interactions are not silently dropped.
effort <- raw |>
  distinct(site.code, year, week, date, obs.start, obs.end, tot.obs.length) |>
  group_by(site.code, year, week) |>
  summarise(total_minutes = sum(tot.obs.length), .groups = "drop")

# Filter to foraging visits with a recorded plant
frg <- raw |>
  filter(behaviour == "frg", !is.na(plant.species))

# ── Site-level ───────────────────────────────────────────────────────────────

# Interaction counts normalized by total observation minutes
interactions_long <- frg |>
  group_by(site.code, year, week, species, plant.species) |>
  summarise(n_interactions = n(), .groups = "drop") |>
  left_join(effort, by = c("site.code", "year", "week")) |>
  mutate(interaction_rate = n_interactions / total_minutes)

write_csv(interactions_long, "data/processed/interactions_long.csv")

# Build a bee × plant matrix from a long-format slice
build_matrix <- function(df) {
  df |>
    select(species, plant.species, interaction_rate) |>
    pivot_wider(
      names_from  = plant.species,
      values_from = interaction_rate,
      values_fill = 0
    ) |>
    tibble::column_to_rownames("species") |>
    as.matrix()
}

site_keys <- interactions_long |>
  group_by(site.code, year, week) |>
  group_keys() |>
  mutate(name = paste(site.code, year, week, sep = "_"))

weekly_matrices <- interactions_long |>
  group_by(site.code, year, week) |>
  group_split() |>
  setNames(site_keys$name) |>
  lapply(build_matrix)

saveRDS(weekly_matrices, "data/processed/weekly_matrices.rds")

# ── Combined (all sites pooled) ───────────────────────────────────────────────

effort_combined <- raw |>
  distinct(site.code, year, week, date, obs.start, obs.end, tot.obs.length) |>
  group_by(year, week) |>
  summarise(total_minutes = sum(tot.obs.length), .groups = "drop")

interactions_combined_long <- frg |>
  group_by(year, week, species, plant.species) |>
  summarise(n_interactions = n(), .groups = "drop") |>
  left_join(effort_combined, by = c("year", "week")) |>
  mutate(interaction_rate = n_interactions / total_minutes)

combined_keys <- interactions_combined_long |>
  group_by(year, week) |>
  group_keys() |>
  mutate(name = paste(year, week, sep = "_"))

weekly_matrices_combined <- interactions_combined_long |>
  group_by(year, week) |>
  group_split() |>
  setNames(combined_keys$name) |>
  lapply(build_matrix)

saveRDS(weekly_matrices_combined, "data/processed/weekly_matrices_combined.rds")

# ── Summary ───────────────────────────────────────────────────────────────────

message("Done.")
message("  interactions_long rows:  ", nrow(interactions_long))
message("  Site-level matrices:     ", length(weekly_matrices))
message("  Combined matrices:       ", length(weekly_matrices_combined))
message("  Saved to data/processed/")
