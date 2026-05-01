# Feasibility & Structural Stability — Project Ideas

## Data Overview
- 9 *Bombus* species × 110 plant species
- 6 sites (AW, CC, FP, GS, RH, SP) × 9 years (2015–2023)
- Weekly temporal resolution; habitat types: aspen, dry, wet
- Key processed file: `data/bee_flw_data.csv`

---

## 1. Temporal Trends in Structural Stability (Core Analysis)

**Framework:** Saavedra 2024, Cenci & Saavedra 2018

Build annual interaction matrices per site-year and compute the feasibility domain (Ω) for each.

**Steps:**
1. Construct bee × plant interaction matrices from `bee_flw_data.csv` — entries = interaction frequency normalized by `tot.obs.length`
2. Compute Ω (solid angle of feasibility cone) for each site-year matrix
3. Plot Ω ~ year + site to detect directional trends over 2015–2023
4. Test whether habitat type (aspen/dry/wet) moderates stability

**Key question:** Is structural stability of RMBL bumble bee communities declining, stable, or variable over time?

---

## 2. Structural Stability as a Predictor of Phenology

**Framework:** Song & Saavedra 2018

Direct extension of their high-arctic study to RMBL. Data has `doy` and `week` at 6 sites × 9 years.

**Steps:**
1. Compute Ω per site-year from interaction matrices
2. Build weekly species richness time series from `bee_flw_data.csv` (bee side) and `flw_data.csv` (plant side)
3. Regress weekly richness changes against Ω, floral abundance (`flw.per.meter.sum.week`), and weather
4. Test whether Ω is the most consistent predictor across sites and years

**Key question:** Does structural stability predict the timing and magnitude of seasonal species turnover at RMBL?

---

## 3. Nonlinear Structural Stability with Floral Abundance

**Framework:** Cenci & Saavedra 2018

Floral abundance data (`flw.per.meter.sum.week`) allows modeling saturating functional responses rather than assuming linear LV dynamics.

**Steps:**
1. Fit Type II (Holling) functional response curves for each bee–plant pair using foraging observations vs. floral abundance
2. Compute structural stability under the nonlinear model
3. Compare Ω estimates from linear vs. nonlinear models across site-years
4. Identify bee–plant pairs where nonlinearity materially changes stability estimates

**Key question:** Does accounting for resource saturation change inferences about community stability?

---

## 4. Rapid Persistence Monitoring via Motifs

**Framework:** Song et al. 2023

Useful given that some site-years likely have incomplete sampling. Tests whether small subnetworks predict whole-community persistence.

**Steps:**
1. Identify motifs (e.g., individual bee species + their top 2–3 plant resources) across the 9-year record
2. Assess persistence of each motif in isolation per site-year
3. Correlate motif persistence with whole-network Ω estimates
4. Determine minimum sampling effort needed to reliably predict community persistence

**Key question:** Which bee–plant subnetworks are the best early-warning indicators of community decline at RMBL?

---

## 5. Nestedness, Habitat, and Seasonality

**Framework:** Song et al. 2017

**Steps:**
1. Compute NODF nestedness for each site-year interaction matrix
2. Compare nestedness across habitat types (aspen/dry/wet) and years
3. Obtain temperature variability data for RMBL (available from the site weather station) and test the nestedness ~ seasonality relationship
4. Apply Song et al.'s comparative statistics (z-scores) to allow valid cross-network comparison

**Key question:** Do RMBL networks in more variable or seasonal microhabitats show higher nestedness, consistent with theory?

---

## 6. Assembly Graph Across Years

**Framework:** Song 2024

**Steps:**
1. Define nodes as species assemblages per site-year (presence/absence of each bee species)
2. Draw edges between consecutive years at the same site (species gained/lost)
3. Identify whether communities converge to stable assemblages or show path-dependent (priority-effect) trajectories
4. Compare assembly paths across sites

**Key question:** Is community assembly at RMBL deterministic and convergent, or history-dependent across sites?

---

## 7. Geometric Beta Diversity Across Sites and Years

**Framework:** Song et al. 2025

**Steps:**
1. Compute geometric beta diversity (hypervolume of community embedding) for bee and plant communities separately
2. Partition beta diversity across: sites, habitat types, years
3. Test whether beta diversity changes directionally over 2015–2023
4. Compare geometric measure to classic metrics (Bray-Curtis, Jaccard) to identify novel patterns

**Key question:** Is compositional variation among RMBL sites and habitats increasing or decreasing over time?

---

## 8. Model Validation with Covariance Criteria

**Framework:** Song & Levine 2025

The 9-year time series enables rigorous model testing.

**Steps:**
1. Extract annual bee abundance time series per species per site
2. Test covariance structure against predictions of competing interaction models (LV linear vs. nonlinear, with/without higher-order interactions)
3. Identify which models are ruled out by the covariance criteria
4. Build confidence in the model used for structural stability computation (links back to Analysis 1)

**Key question:** Which ecological interaction model is best supported by the RMBL time-series data?

---

## Suggested Priority Order

| Priority | Analysis | Rationale |
|----------|----------|-----------|
| 1 | Temporal trends in Ω (#1) | Core question; straightforward from existing data |
| 2 | Structural stability & phenology (#2) | Near-direct replication of Song & Saavedra 2018 |
| 3 | Rapid persistence monitoring (#4) | High conservation relevance; practical output |
| 4 | Nonlinear stability (#3) | Requires functional response fitting; more involved |
| 5 | Nestedness & seasonality (#5) | Needs external temperature data |
| 6 | Assembly graph (#6) | Exploratory; good complement to stability analyses |
| 7 | Geometric beta diversity (#7) | Standalone methodology question |
| 8 | Model validation (#8) | Most technically demanding; validates assumptions of #1 |
