# analysis/02_feasibility/01_compute_omega.R
# Computes feasibility domain Ω and rescaled structural stability ω for each
# weekly bee × plant interaction matrix.
#
# Method: Song & Saavedra (2018) Proc. R. Soc. B 285:20180767
#   - Binarize observed interaction matrix M
#   - Build full S×S mutualistic community matrix A
#       a_ij = γ * m_ij / k_i^δ  (off-diagonal mutualistic effects)
#       a_ii = -1                  (self-regulation; no intra-trophic competition)
#       δ = 0.5  (mutualistic trade-off; robust to values in (0,1))
#       γ = 1 / spectral_radius(normalized off-diagonal matrix)
#   - Σ = 2 * (AᵀA)⁻¹
#   - Ω = P(X ≥ 0), X ~ N(0, Σ)  [quasi-Monte Carlo via mvtnorm::pmvnorm]
#   - ω = Ω^(1/S)                  [rescaled, dimension-corrected; ∈ (0,1)]
#
# Inputs:  data/processed/weekly_matrices.rds
#          data/processed/weekly_matrices_combined.rds
# Outputs: data/processed/omega_site.csv
#          data/processed/omega_combined.csv

library(readr)
library(dplyr)
library(mvtnorm)

DELTA <- 0.5  # mutualistic trade-off (Song & Saavedra 2018)

# ── Community matrix construction ─────────────────────────────────────────────

build_community_matrix <- function(M_bee_plant) {
  # M_bee_plant: rows = bee species, cols = plant species, values = interaction_rate
  # Returns list(A = S_eff×S_eff community matrix, S_eff = effective species count),
  # or NULL if degenerate.
  #
  # Species with identical binary interaction profiles occupy the same structural
  # position in the LV model (their column vectors in A are identical, making A
  # rank-deficient). We collapse duplicates to unique profiles before building A,
  # reducing S to S_eff. ω is computed on S_eff; S (actual richness) is tracked
  # separately.

  # Binarize: any positive rate → 1 (method uses presence/absence of interactions)
  M_bin <- (M_bee_plant > 0) * 1

  # Drop any species with zero interactions after binarization (safeguard)
  M_bin <- M_bin[rowSums(M_bin) > 0, colSums(M_bin) > 0, drop = FALSE]

  # Collapse species with identical interaction profiles (duplicate rows = bees,
  # duplicate cols = plants). Duplicates create linearly dependent columns in A.
  M_bin <- M_bin[!duplicated(M_bin),      , drop = FALSE]  # unique bee profiles
  M_bin <- M_bin[, !duplicated(t(M_bin)),   drop = FALSE]  # unique plant profiles

  n_b <- nrow(M_bin)  # unique bee structural positions
  n_p <- ncol(M_bin)  # unique plant structural positions
  S   <- n_b + n_p    # S_eff: effective species count

  if (S < 2) return(NULL)

  # Degrees
  k_b <- rowSums(M_bin)  # number of plant partners per bee
  k_p <- colSums(M_bin)  # number of bee partners per plant

  # Normalized interaction blocks (without γ):
  # Effect of plants on bee i:  row i of M_bin divided by k_b[i]^δ
  N_b <- sweep(M_bin,    1, k_b^DELTA, "/")   # n_b × n_p
  # Effect of bees on plant j:  row j of t(M_bin) divided by k_p[j]^δ
  N_p <- sweep(t(M_bin), 1, k_p^DELTA, "/")   # n_p × n_b

  # Full off-diagonal block matrix B, species order: [bees, plants]
  # [bees  × bees  = 0,   bees  × plants = N_b]
  # [plants × bees = N_p, plants × plants = 0 ]
  B <- rbind(
    cbind(matrix(0, n_b, n_b), N_b),
    cbind(N_p,                  matrix(0, n_p, n_p))
  )

  # γ = maximum mutualistic strength without losing dynamical stability
  # (Song & Saavedra 2018: γ_max = 1/ρ(B))
  # At γ = γ_max exactly, A has a zero eigenvalue (det(A^T A) = 0), making the
  # feasibility integral undefined. A small regularization (1 - 1e-6) moves γ
  # just inside the stability boundary; the limit is stable to < 0.01% change
  # from 1e-4 to 1e-6 regularization.
  rho <- max(Re(eigen(B, symmetric = FALSE, only.values = TRUE)$values))
  if (!is.finite(rho) || rho <= 0) return(NULL)
  gamma <- (1 - 1e-6) / rho

  # Community matrix A = γB − I
  A <- gamma * B - diag(S)
  list(A = A, S_eff = S)
}

# ── Ω and ω computation ───────────────────────────────────────────────────────

compute_omega <- function(mat_result) {
  if (is.null(mat_result)) return(c(Omega = NA_real_, omega = NA_real_, S_eff = NA_integer_))

  A     <- mat_result$A
  S_eff <- mat_result$S_eff
  AtA   <- t(A) %*% A

  Sigma <- tryCatch(solve(AtA), error = function(e) NULL)
  if (is.null(Sigma)) return(c(Omega = NA_real_, omega = NA_real_, S_eff = S_eff))
  Sigma <- (Sigma + t(Sigma)) / 2  # enforce symmetry against numerical drift

  result <- tryCatch(
    as.numeric(mvtnorm::pmvnorm(
      lower     = rep(0, S_eff),
      upper     = rep(Inf, S_eff),
      mean      = rep(0, S_eff),
      sigma     = Sigma,
      algorithm = mvtnorm::GenzBretz(maxpts = 1e6, abseps = 1e-4, releps = 0)
    )),
    error = function(e) NA_real_
  )

  Omega <- result
  omega <- if (is.finite(Omega) && Omega > 0) Omega^(1 / S_eff) else NA_real_
  c(Omega = Omega, omega = omega, S_eff = S_eff)
}

# ── Helper: apply to a named list of matrices ─────────────────────────────────

omega_from_list <- function(mat_list, key_parser) {
  lapply(names(mat_list), function(nm) {
    mat_result <- build_community_matrix(mat_list[[nm]])
    vals       <- compute_omega(mat_result)
    row        <- key_parser(nm)
    row$n_bee   <- nrow(mat_list[[nm]])
    row$n_plant <- ncol(mat_list[[nm]])
    row$S       <- row$n_bee + row$n_plant          # actual species richness
    row$S_eff   <- as.integer(vals["S_eff"])        # structural positions (after collapsing duplicates)
    row$Omega   <- vals["Omega"]
    row$omega   <- vals["omega"]
    as.data.frame(row, stringsAsFactors = FALSE)
  }) |> bind_rows()
}

# ── Site-level matrices ───────────────────────────────────────────────────────

weekly_matrices <- readRDS("data/processed/weekly_matrices.rds")

parse_site_key <- function(nm) {
  parts <- strsplit(nm, "_")[[1]]
  list(site = parts[1], year = as.integer(parts[2]), week = as.integer(parts[3]))
}

omega_site <- omega_from_list(weekly_matrices, parse_site_key)
write_csv(omega_site, "data/processed/omega_site.csv")

message("Site-level Ω done: ", nrow(omega_site), " matrices")
message("  NA Omega: ", sum(is.na(omega_site$Omega)))
message("  omega range: [",
        round(min(omega_site$omega, na.rm = TRUE), 4), ", ",
        round(max(omega_site$omega, na.rm = TRUE), 4), "]")

# ── Combined (all-sites pooled) matrices ──────────────────────────────────────

weekly_matrices_combined <- readRDS("data/processed/weekly_matrices_combined.rds")

parse_combined_key <- function(nm) {
  parts <- strsplit(nm, "_")[[1]]
  list(year = as.integer(parts[1]), week = as.integer(parts[2]))
}

omega_combined <- omega_from_list(weekly_matrices_combined, parse_combined_key)
write_csv(omega_combined, "data/processed/omega_combined.csv")

message("Combined Ω done: ", nrow(omega_combined), " matrices")
message("  NA Omega: ", sum(is.na(omega_combined$Omega)))
message("  omega range: [",
        round(min(omega_combined$omega, na.rm = TRUE), 4), ", ",
        round(max(omega_combined$omega, na.rm = TRUE), 4), "]")
