# Writes two aggregate tables of the main simulation by the sign of the
# clustering coefficient (gwesp; gwdsp in mutual_gwdsp), from the per-fit
# export of the research repository (data/fits/, data/samples/; no CHPC
# needed):
#
#   data/sim_compare_sign.csv          headline comparison (as sim_compare's
#                                      "all" row) by clustering sign
#   data/sim_paired_density_sign.csv   paired outcomes of the two fits of a
#                                      sample, by clustering sign, M and
#                                      observed mean density
#
#   Rscript scripts/sim-by-sign.R --repo=<research repo> --out=data
#
# Also sourced by scripts/tables.R. Definitions follow report/analysis.R:
# convergence is the share of GW coefficients whose fit converged (all
# attempted fits); time is the median of converged fits; bias, MAE and
# coverage are over the GW coefficients of converged fits. Fits that failed
# with a file error of the node ("I/O error") are not fits and are dropped;
# paired tables need both fits of a sample. Only aggregates are written.

if (!exists("repo")) {
  args <- commandArgs(trailingOnly = TRUE)
  arg  <- function(name, default) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
  }
  repo <- normalizePath(arg("repo", "../../research/ergm-cond-consistency"))
  out  <- arg("out", "data")
}

z95 <- stats::qnorm(0.975)
read_dir <- function(dir) {
  files <- list.files(file.path(repo, "data", dir), "\\.csv$", full.names = TRUE)
  data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
}
fits <- as.data.frame(read_dir("fits"))
smp  <- as.data.frame(read_dir("samples"))
fits <- fits[fits$outcome != "I/O error", ]
fits$sample <- paste(fits$experiment, fits$M, fits$r, sep = ":")
smp$sample  <- paste(smp$experiment, smp$M, smp$r, sep = ":")

# Clustering sign of each sample.
clus_theta <- ifelse(is.na(smp$theta_gwesp), smp$theta_gwdsp, smp$theta_gwesp)
smp$clustering <- ifelse(clus_theta > 0, "positive", "negative")
fits <- merge(
  fits,
  smp[c("sample", "clustering", "mean_density")],
  by = "sample"
)
gw <- fits[fits$role == "GW", ]
fit1 <- gw[!duplicated(gw[c("sample", "model")]), ]

# Samples with both fits.
both <- intersect(
  fit1$sample[fit1$model == "cond"],
  fit1$sample[fit1$model == "uncond"]
)

compare_row <- function(g, f, label) {
  s <- function(m) {
    x  <- g[g$model == m, ]
    ok <- x[x$ok, ]
    e  <- ok$est - ok$truth
    c(
      conv = mean(x$ok),
      time = stats::median(f$time[f$model == m & f$ok]),
      bias = stats::median(e),
      mae  = stats::median(abs(e)),
      cov  = mean(abs(e) <= z95 * ok$se)
    )
  }
  a <- s("cond")
  b <- s("uncond")
  data.frame(
    clustering     = label,
    samples        = length(intersect(unique(g$sample), both)),
    conv_cond      = a[["conv"]],
    conv_uncond    = b[["conv"]],
    time_ok_cond   = a[["time"]],
    time_ok_uncond = b[["time"]],
    bias_cond      = a[["bias"]],
    bias_uncond    = b[["bias"]],
    mae_cond       = a[["mae"]],
    mae_uncond     = b[["mae"]],
    cov_cond       = a[["cov"]],
    cov_uncond     = b[["cov"]]
  )
}
sim_compare_sign <- rbind(
  compare_row(gw[gw$clustering == "positive", ], fit1[fit1$clustering == "positive", ], "positive"),
  compare_row(gw[gw$clustering == "negative", ], fit1[fit1$clustering == "negative", ], "negative"),
  compare_row(gw, fit1, "all")
)
utils::write.csv(
  sim_compare_sign,
  file.path(out, "sim_compare_sign.csv"),
  row.names = FALSE
)
cat("wrote", file.path(out, "sim_compare_sign.csv"), "\n")

# Paired outcomes (convergence of each fit) by sign, M and density.
w <- merge(
  fit1[fit1$model == "cond", c("sample", "clustering", "M", "mean_density", "ok")],
  fit1[fit1$model == "uncond", c("sample", "ok")],
  by       = "sample",
  suffixes = c(".c", ".u")
)
w$dband <- cut(
  w$mean_density,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
  labels         = c("0–0.1", "0.1–0.2", "0.2–0.3", "0.3–0.4", "0.4–0.5", "0.5+"),
  include.lowest = TRUE
)
sim_paired_density_sign <- do.call(rbind, lapply(
  split(w, w[c("clustering", "M", "dband")], drop = TRUE),
  function(x) data.frame(
    clustering  = x$clustering[1],
    M           = x$M[1],
    dband       = as.character(x$dband[1]),
    samples     = nrow(x),
    both        = mean(x$ok.c & x$ok.u),
    only_cond   = mean(x$ok.c & !x$ok.u),
    only_uncond = mean(!x$ok.c & x$ok.u),
    neither     = mean(!x$ok.c & !x$ok.u)
  )
))
utils::write.csv(
  sim_paired_density_sign,
  file.path(out, "sim_paired_density_sign.csv"),
  row.names = FALSE
)
cat("wrote", file.path(out, "sim_paired_density_sign.csv"), "\n")
