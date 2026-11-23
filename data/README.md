# data/

Aggregate tables behind every table, figure and result number of the deck
(`scripts/deck.R` reads them for `index.qmd`, `scripts/figures.R` for the
figures). Only aggregates: shares, medians and counts over simulated samples.

The simulation results live in the research repository
(`../../research/ergm-cond-consistency`, github.com/gvegayon/ergm-cond-consistency);
the full results are on CHPC.

| file | content | built by |
|---|---|---|
| `sim_compare.csv` | conditional vs unconditional, by experiment, M and pooled (report Table 2); `time_ok_*` (median time of converged fits) only from `tables.R` | both |
| `sim_by_density.csv` | by observed density (report Tables 3c-9c) | both |
| `sim_failures_by_M.csv` | causes of failure by M (report Tables 3b-9b) | both |
| `sim_dense.csv` | densities above 0.5, earlier design (report Table A1) | both |
| `info_loss.csv` | information lost by conditioning (loglik report, section 5) | both |
| `bridges.csv` | conditional-bridge calibration (`experiment-loglik/calibration/summary.csv`) | both |
| `sim_paired.csv` | paired outcomes; P(conditional converges \| unconditional fails) | `tables.R` |
| `sim_levels.csv` | returned / finite / well-defined / accurate fits, by model | `tables.R` |
| `sim_discordant_causes.csv` | when one model converges, why the other failed | `tables.R` |
| `info_mutual_gwesp_bins.csv` | `mutual_gwesp` information loss by density and clustering sign (median, 10th/90th percentile) | `tables.R` |
| `info_gwesp_sign.csv` | `mutual_gwesp` information loss and non-identification by clustering sign | `tables.R` |
| `info_by_design.csv` | information lost on each GW coefficient by design and sign of the coefficient (median, 95% CI of the median, share not identified), M = 1 and 5 | `scripts/info-by-design.R` (also run by `tables.R`; no CHPC needed) |
| `sim_compare_sign.csv` | headline comparison (as the pooled row of `sim_compare.csv`) by sign of the clustering coefficient | `scripts/sim-by-sign.R` (also run by `tables.R`; no CHPC needed) |
| `sim_paired_density_sign.csv` | paired convergence outcomes by clustering sign, M and observed density | `scripts/sim-by-sign.R` |
| `sim_quality.csv` | usable and accurate GW coefficients, by clustering sign (report "Beyond convergence", Table 12) | `tables.R` |

## Building

**Authoritative: `scripts/tables.R`**, from the fits, on CHPC (the results
are there):

```sh
module load R/4.4.2
cd <research repo>
srun -A notchpeak-shared-short -p notchpeak-shared-short \
  --qos=notchpeak-shared-short -n 1 -c 2 --mem=8G -t 01:00:00 \
  Rscript <deck>/scripts/tables.R --repo=. --out=<deck>/data
```

It needs `results/analysis/fits.rds` (from `report/prepare.R`),
`results-density09/analysis/fits.rds` and `experiment-loglik/results-v3/`.
Copy `data/` back, then `Rscript scripts/figures.R` and `quarto render`.

**Without CHPC: `scripts/tables-from-reports.R`** reads the tables the two
rendered reports print (`report/report.md`, `experiment-loglik/report.md`)
and writes the files marked "both", with the reports' rounding. It cannot
build the `tables.R`-only files; the slides that need them show a notice
until `tables.R` has run.

```sh
Rscript scripts/tables-from-reports.R   # from the deck's root
```

The files in this folder were last built on 2026-11-20 by `tables.R`, from
the research repository's exported per-fit results (`data/fits/`,
`data/samples/`, all 28,000 fits), stacked into a `fits.rds` with the
columns of `report/prepare.R`. `info_loss.csv`, `sim_dense.csv` and
`bridges.csv` were not rebuilt (their inputs did not change).
`info_mutual_gwesp_bins.csv` and `info_gwesp_sign.csv` were built on
2026-11-21 from the exported loglik results (`data/loglik/`, `data/samples/`);
recomputing `info_loss.csv` from the same export reproduces it.

`fig/info-mutual-gwesp.png` is the one figure drawn from per-sample values:
`scripts/figures.R` reads the research repo's `data/loglik/` and
`data/samples/` in place and writes only the PNG; no per-sample file is
copied here.
