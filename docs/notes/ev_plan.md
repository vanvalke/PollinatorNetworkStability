# Project Plan

Look at the how the structure of the networks through time contribute to the “ecological feasibility” of the plants and pollinators working within them (i.e., the ability to maintain positive abundances with perturbation).

## Research plan

### Exploratory analysis

- Repeat the analysis at both the individual site level (repeat for each site) and with all sites combined
- Build interaction networks for each week
- Calculate the feasibility domain (Ω) per network
- Plot Ω across weeks within each year to test for the hump-shaped seasonal pattern (H1)
- Plot annual Ω trajectories across 2015–2023 to assess directional trend and year-to-year resilience (H2)

### Incorporating environmental data

- Extract environmental data from the RMBL
  - at the annual level
    - snowfall
    - snowmelt date (primary candidate for H3)
  - at the site level
    - elevation
    - surrounding habitat
    - potential other factors of interest
  - at the week level
    - mean temp
    - max temp
    - min temp
    - temp range (primary candidate for H3)
    - precipitation
    - other important contributions to pollinator activity
- Community context data
  - total pollinators
  - diversity and abundance of floral resources
    - presumably these differ throughout the season and at each site
- Model Ω as a function of environmental predictors (H3); use model selection (AIC) to identify which variables explain the most variance
- Test the environment × structure interaction (H4): fit models with nestedness × temperature range interaction term and assess whether the effect of nestedness on Ω changes across thermal conditions

### Adaptation space analysis

- For each site-year, compute the adaptation space for all subsets of bee species (or focus on individual species + their plant partners as the relevant group)
- Compare adaptation space size to species' presence/absence record across 2015–2023 (H5)
- Quantify diet breadth per bee species (number of plant species visited, weighted by visit frequency) and test its association with adaptation space size (H6)
- Identify which bee species have the smallest adaptation spaces — these are the most conservation-relevant targets

## Questions and hypotheses

### Q1: How does the feasibility domain change within and across seasons?
- **H1:** The feasibility domain forms a hump shape across the season — rising as species accumulate and interactions diversify in early summer, then declining as species drop out toward season's end. This reflects the phenological arc of the community and predicts that mid-season networks are the most robust to perturbation.
- **H2:** Rather than being simply consistent across years, the feasibility domain shows **resilience** — recovering to a similar mid-season peak after low years — but may exhibit a directional trend (decline or increase) over 2015–2023 that reflects long-term community change. Testing for both resilience and trend distinguishes a stable, buffered system from one undergoing directional change.

### Q2: How do environmental conditions shape the feasibility domain — and does this depend on network structure?
Following Cenci et al. (2018), the importance of a given network structure cannot be evaluated independently of the environmental context in which it operates. The same structural pattern may increase feasibility under one set of conditions and have no effect (or even reduce it) under another.
- **H3:** Specific environmental axes — particularly **snowmelt timing** and **weekly temperature range** — explain more variance in feasibility than other variables, because they constrain both the timing of species appearances and the breadth of foraging conditions.
- **H4:** The relationship between network structure (e.g., nestedness, connectance) and feasibility is **environment-dependent**: nested structures increase feasibility in thermally variable weeks but provide little advantage in stable, warm conditions. This directly tests whether structural patterns have consistent importance or only context-dependent importance.

### Q3: Which species are most buffered against environmental change?
Drawing on the adaptation space framework (Cenci et al. 2018), different subsets of species within the community will have larger or smaller adaptation spaces — the range of environmental conditions they can remain persistent in through interaction reorganization.
- **H5:** Bee species that persist across all 9 years occupy a larger **adaptation space** than species with more variable presence, because their interaction flexibility allows them to compensate for environmental shifts by reorganizing which plant partners they rely on.
- **H6:** Adaptation space size is positively associated with diet breadth (number of plant species visited), such that generalist bee species are more buffered against environmental change than specialists.
