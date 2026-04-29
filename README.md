# Structural Stability of Plant-Pollinator Networks

by Ethan VanValkenburg and Paul CaraDonna

start: March 2024  
target finish: August 2024

## Description

This repository supports work on temporal dynamics and stability-related questions in plant–pollinator networks, using long-term bumble bee monitoring and floral transect data from the Ogilvie & CaraDonna project at the Rocky Mountain Biological Laboratory (RMBL), Gothic, CO.

Data are sourced from the Open Science Framework ([OSF project zq36h](https://osf.io/zq36h/)). For methods context, see Ogilvie & CaraDonna (2022), *Journal of Animal Ecology*: https://doi.org/10.1111/1365-2656.13825.

### Repository layout

| Path | Purpose |
|------|--------|
| `analysis/download_and_prepare_bee_flower_data.R` | Downloads OSF bee and flower CSVs, applies cleaning/summaries identical to the starter script, writes snapshots under `data/`. |
| `rmbl.bee.flw.starter.script.R` | Original lightweight script: reads the same URLs in memory only (no local CSV outputs). |
| `data/raw_osf/` | Cached copies of the three downloaded OSF tables (bee abundance; flower transects 2016–2023; flower transects 2015-only). |
| `data/` | Processed outputs from the analysis script (cleaned bee table, aggregated floral table, joined bee–flower table, plus transect summary before week rollup). |

Processed outputs currently span **nine field seasons (2015–2023)** after filtering and joins.
