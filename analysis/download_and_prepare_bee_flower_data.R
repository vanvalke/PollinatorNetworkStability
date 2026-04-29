#### Download OSF bee + floral data and prepare analytic tables ####
# Mirrors rmbl.bee.flw.starter.script.R (same URLs, filters, summaries, join).
# Run with working directory set to the project root (PollinatorNetworkStability).

#### Data citation / overview ####

# OSF project: https://osf.io/zq36h/
# Ogilvie, J. E., & CaraDonna, P. J. (2022). Journal of Animal Ecology, 91, 2412–2423.

#### Packages ####
library(tidyverse)

#### Paths ####
data_dir <- "data"
raw_dir <- file.path(data_dir, "raw_osf")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

url_bee <- "https://osf.io/hmgfe/download"
url_flw2016plus <- "https://osf.io/r4ngx/download"
url_flw2015 <- "https://osf.io/k342u/download"

#### Download (read URL) and save raw snapshots ####

bee.data.raw <- read_csv(url_bee, show_col_types = FALSE)
readr::write_csv(bee.data.raw, file.path(raw_dir, "bee_abundance_raw.csv"))

flwtrans <- read_csv(url_flw2016plus, show_col_types = FALSE)
readr::write_csv(flwtrans, file.path(raw_dir, "flower_transects_2016_2023_raw.csv"))

flwtrans2015 <- read_csv(url_flw2015, show_col_types = FALSE)
readr::write_csv(flwtrans2015, file.path(raw_dir, "flower_transects_2015_raw.csv"))

#### Bumble bee data manipulation (same as starter script) ####

bee.data <- bee.data.raw %>%
  dplyr::mutate(species = stringr::str_replace(species, "rufocinctus.dark", "rufocinctus"),
                species = stringr::str_replace(species, "rufocinctus.orange", "rufocinctus")) %>%
  dplyr::filter(caste %in% c("q", "w", "m", "old.q"),
                behaviour %in% c("frg", "frg+ns", "ns", "ft", "rob.prim","rob.sec","rob.base"),
                species %in% c("appositus", "bifarius", "californicus", "flavifrons", "nevadensis",
                               "mixtus", "occidentalis", "rufocinctus", "insularis"),
                site.code %in% c("AW", "CC", "FP", "GS", "RH", "SP")) %>%
  dplyr::mutate(caste = stringr::str_replace(caste, "old.q", "q")) %>%
  dplyr::select(site:resource.coll) %>%
  as_tibble()

#### Floral resources (same as starter script) ####

flw.summary <- flwtrans %>%
  dplyr::group_by(habitat, site, site.code, year, week, doy, plant.species, transect.section) %>%
  dplyr::summarize(flw.count.mean = mean(flower.count),
                   flw.count.sum = sum(flw.count.mean),
                   flw.count.mean.per.meter = (sum(flw.count.mean/(20*0.5))))

flw.summary.2015 <- flwtrans2015 %>%
  dplyr::filter(transect.section != "habitat") %>%
  dplyr::group_by(habitat, site, site.code, year, week, doy, plant.species, transect.section) %>%
  dplyr::summarize(flw.count.mean = mean(flower.count),
                   flw.count.sum = sum(flw.count.mean),
                   flw.count.mean.per.meter = (sum(flw.count.mean/(5*0.5))))

flw.summary.all.years <- dplyr::bind_rows(flw.summary, flw.summary.2015) %>%
  dplyr::filter(!plant.species %in%
                  c("Androsace.septentrionalis",
                    "Artemisia.tridentata",
                    "Boechera.stricta",
                    "Descurainia.incana",
                    "Epilobium.sp",
                    "Galium.septentrionalis",
                    "Galium.septentrionale",
                    "Oligosporus.dracunculus",
                    "Polygonum.douglasii",
                    "Tragopogon.dubius"),
                site.code %in% c("AW", "CC", "FP", "GS", "RH", "SP"))

flw.data <- flw.summary.all.years %>%
  dplyr::filter(plant.species != "0") %>%
  dplyr::group_by(year, site, site.code, plant.species, week) %>%
  dplyr::summarize(flw.per.meter.sum.week = sum(flw.count.mean.per.meter),
                   flw.count.sum.week = sum(flw.count.sum))

#### Join bee + floral ####

bee.flw.data <- bee.data %>%
  dplyr::left_join(flw.data)

#### Save processed tables ####

readr::write_csv(bee.data, file.path(data_dir, "bee_data.csv"))
readr::write_csv(flw.data, file.path(data_dir, "flw_data.csv"))
readr::write_csv(bee.flw.data, file.path(data_dir, "bee_flw_data.csv"))
readr::write_csv(flw.summary.all.years, file.path(data_dir, "flw_summary_all_years.csv"))

message("Saved raw OSF downloads under: ", normalizePath(raw_dir))
message("Saved processed CSVs under: ", normalizePath(data_dir))

print(bee.flw.data)
