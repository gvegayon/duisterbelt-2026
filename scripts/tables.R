# Writes every table the deck shows, as aggregate CSV files in data/, from
# the simulation results of the research repository
# (github.com/gvegayon/ergm-cond-consistency):
#
#   results/analysis/fits.rds               main experiment (report/prepare.R)
#   results-density09/analysis/fits.rds     earlier design, densities to 0.9
#   experiment-loglik/results-v3/<design>/  information at the true parameter
#   experiment-loglik/calibration/summary.csv  conditional bridges
#
# The results live on CHPC, so run this there, in an srun (it reads every
# sample of the loglik experiment):
#
#   module load R/4.4.2
#   srun -A notchpeak-shared-short -p notchpeak-shared-short \
#     --qos=notchpeak-shared-short -n 1 -c 2 --mem=8G -t 01:00:00 \
#     Rscript <deck>/scripts/tables.R --repo=<repo> --out=<deck>/data
#
# and copy data/ back. Only aggregates are written: shares, medians and
# counts over samples, never a fit or a network. Definitions follow
# report/analysis.R and experiment-loglik/report.qmd, which are sourced, so
# the numbers match the two reports.
#
# Run without the CHPC results, scripts/tables-from-reports.R rebuilds the
# subset of these files that the two reports already print (see data/README.md).

args <- commandArgs(trailingOnly = TRUE)
arg  <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
repo <- normalizePath(arg("repo", "../../research/ergm-cond-consistency"))
out  <- arg("out", "data")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

write_csv <- function(x, name) {
  utils::write.csv(x, file.path(out, paste0(name, ".csv")), row.names = FALSE)
  cat("wrote", file.path(out, paste0(name, ".csv")), "\n")
}

# Helpers and definitions of the simulation report: exps, fail_types,
# density_bin(), summary_table(), failure_table(), compare_table(),
# mcnemar_p().
source(file.path(repo, "report", "analysis.R"))

# A fit's coefficients are "well defined" when every standard error is below
# this bound (log-odds scale), and "close to the truth" when no GW
# coefficient misses by more than WILD (the `wild` default of
# condergm::summarize_experiment()).
SE_MAX <- 5
WILD   <- 3

# ---------------------------------------------------------------------------
# Main experiment
# ---------------------------------------------------------------------------
res <- readRDS(file.path(repo, "results", "analysis", "fits.rds"))
res <- res[!grepl("_n[0-9]+$", res$experiment) & res$method == "MCMLE", ]
res <- res[res$type != "I/O error", ]
res$experiment <- factor(res$experiment, exps)
res$model <- factor(res$model, c("uncond", "cond"))
res$dband <- density_bin(res$mean_density)
res$all <- "all"

# Conditional vs unconditional, by experiment, by M and overall (Table 2 of
# the report).
cmp <- list(
  compare_table(res, "experiment"),
  compare_table(res, "M"),
  compare_table(res, "all")
)
cmp[[1]]$experiment <- as.character(cmp[[1]]$experiment)
cmp[[2]]$M <- paste0("M", cmp[[2]]$M)
cmp <- do.call(rbind, lapply(cmp, function(x) {
  names(x)[1] <- "group"
  x
}))
# Median time of the converged fits only. The report's time (over all fits,
# timeouts at the limit) mixes speed with how often a model times out; this
# one is the speed of a fit that works.
fit1 <- res[!duplicated(res[c("sample", "arm")]) & res$ok, ]
time_ok <- function(f, m) stats::median(f$time[f$model == m])
grp <- c(
  split(fit1, as.character(fit1$experiment)),
  split(fit1, paste0("M", fit1$M)),
  list(all = fit1)
)
cmp$time_ok_cond   <- vapply(grp[cmp$group], time_ok, 1, m = "cond")
cmp$time_ok_uncond <- vapply(grp[cmp$group], time_ok, 1, m = "uncond")
write_csv(cmp, "sim_compare")

# Quality of the GW coefficients beyond convergence (report, "Beyond
# convergence", Table 12): converged, usable (SE <= 1, |est| <= 10) and
# accurate (usable, interval covers the truth and excludes 0), by the sign of
# the clustering coefficient.
write_csv(quality_table(quality_cols(res[res$role == "GW", ])), "sim_quality")

# By observed density, GW coefficients (Tables 3c-9c).
dens <- do.call(rbind, lapply(exps, function(e) {
  x <- summary_table(res[res$experiment == e, ], "dband")
  if (is.null(x)) return(NULL)
  cbind(experiment = e, x)
}))
write_csv(dens, "sim_by_density")

# One row per fit, with the largest SE of the fit and whether any GW
# coefficient is far from the truth.
fits <- res[!duplicated(res[c("sample", "arm")]), ]
se_max <- tapply(res$se, paste(res$sample, res$arm), function(s) {
  if (all(is.finite(s))) max(s) else Inf
})
gw <- res[res$role == "GW", ]
wild <- tapply(
  abs(gw$est - gw$truth) > WILD,
  paste(gw$sample, gw$arm),
  function(w) any(w %in% TRUE)
)
key <- paste(fits$sample, fits$arm)
fits$se_max <- unname(se_max[key])
fits$wild   <- unname(wild[key])

# Causes of failure, by experiment, M and model (Tables 3b-9b).
fail <- do.call(rbind, lapply(exps, function(e) {
  x <- failure_table(fits[fits$experiment == e, ], "M")
  if (is.null(x)) return(NULL)
  cbind(experiment = e, x)
}))
write_csv(fail, "sim_failures_by_M")

# Levels of "converged", per fit:
#   returned   ergm finished and returned estimates (no timeout or error);
#   finite     ... with finite estimates and SEs (the report's convergence);
#   defined    ... and every SE below SE_MAX;
#   accurate   ... and no GW coefficient off by more than WILD.
fits$returned <- fits$type %in% c("ok", "non-finite SE")
fits$finite   <- fits$ok
fits$defined  <- fits$ok & fits$se_max < SE_MAX
fits$accurate <- fits$defined & !fits$wild
levels_by <- function(f, by) {
  g <- split(f, f[c(by, "model")], drop = TRUE)
  do.call(rbind, lapply(g, function(x) {
    data.frame(
      x[1, c(by, "model")],
      fits     = nrow(x),
      returned = mean(x$returned),
      finite   = mean(x$finite),
      defined  = mean(x$defined),
      accurate = mean(x$accurate),
      row.names = NULL
    )
  }))
}
lev <- rbind(
  cbind(group = "all", levels_by(fits, "all")[, -1]),
  {
    x <- levels_by(fits, "dband")
    names(x)[1] <- "group"
    x$group <- paste("density", x$group)
    x
  }
)
lev$se_max <- SE_MAX
lev$wild   <- WILD
write_csv(lev, "sim_levels")

# Paired on the sample: the two models' outcomes side by side.
w <- merge(
  fits[fits$model == "uncond", c("experiment", "M", "sample", "dband", "type", "ok", "defined")],
  fits[fits$model == "cond", c("sample", "type", "ok", "defined")],
  by       = "sample",
  suffixes = c(".u", ".c")
)
w$all <- "all"
paired_by <- function(x, by, ok_u, ok_c) {
  g <- split(x, x[by], drop = TRUE)
  do.call(rbind, lapply(g, function(d) {
    u <- d[[ok_u]]
    k <- d[[ok_c]]
    data.frame(
      by           = by,
      group        = as.character(d[[by]][1]),
      samples      = nrow(d),
      both         = sum(u & k),
      only_cond    = sum(!u & k),
      only_uncond  = sum(u & !k),
      neither      = sum(!u & !k),
      # The question of the slide: P(cond converges | uncond does not),
      # and the reverse.
      cond_given_uncond_failed = if (any(!u)) mean(k[!u]) else NA_real_,
      uncond_given_cond_failed = if (any(!k)) mean(u[!k]) else NA_real_,
      p            = mcnemar_p(sum(!u & k), sum(u & !k)),
      row.names    = NULL
    )
  }))
}
# Sign of the sample's clustering coefficient, and whether each fit is usable
# (every GW coefficient converged with SE <= 1 and |est| <= 10), as in
# report/cond-failures.qmd.
q <- quality_cols(res)
q <- q[q$role == "GW", ]
use <- tapply(q$usable, paste(q$sample, q$arm), all)
w$usable.u <- unname(use[paste(w$sample, "uncond")])
w$usable.c <- unname(use[paste(w$sample, "cond")])
w$clustering <- q$clustering[match(w$sample, q$sample)]
paired <- rbind(
  cbind(definition = "finite", rbind(
    paired_by(w, "experiment", "ok.u", "ok.c"),
    paired_by(w, "M", "ok.u", "ok.c"),
    paired_by(w, "dband", "ok.u", "ok.c"),
    paired_by(w, "clustering", "ok.u", "ok.c"),
    paired_by(w, "all", "ok.u", "ok.c")
  )),
  cbind(definition = "defined", rbind(
    paired_by(w, "experiment", "defined.u", "defined.c"),
    paired_by(w, "all", "defined.u", "defined.c")
  )),
  cbind(definition = "usable", rbind(
    paired_by(w, "clustering", "usable.u", "usable.c"),
    paired_by(w, "all", "usable.u", "usable.c")
  ))
)
write_csv(paired, "sim_paired")

# Discordant pairs: when one model converges and the other does not, why the
# other one failed (failure types of report/prepare.R).
why <- function(d, failed_model) {
  type <- if (failed_model == "cond") d$type.c else d$type.u
  tab <- table(factor(type, c(fail_types, "ok")))
  data.frame(
    failed_model = failed_model,
    pairs        = nrow(d),
    type         = names(tab),
    n            = as.integer(tab),
    share        = as.numeric(tab) / max(nrow(d), 1)
  )
}
disc <- rbind(
  why(w[w$ok.u & !w$ok.c, ], "cond"),
  why(w[!w$ok.u & w$ok.c, ], "uncond")
)
write_csv(disc, "sim_discordant_causes")

# ---------------------------------------------------------------------------
# Earlier design: densities above 0.5 (Table A1)
# ---------------------------------------------------------------------------
old_path <- file.path(repo, "results-density09", "analysis", "fits.rds")
if (file.exists(old_path)) {
  old <- readRDS(old_path)
  old <- old[old$start == "shared" & old$method == "MCMLE", ]
  old$model <- factor(old$model, c("uncond", "cond"))
  old$dband <- density_bin(old$mean_density)
  x <- old[old$mean_density >= 0.5 &
             old$role %in% c("GW", "nodematch.x", "mutual"), ]
  x$param <- ifelse(x$role == "GW", "GW terms", "nodematch.x and mutual")
  x <- summary_table(x, "dband", roles = c("GW", "nodematch.x", "mutual"))
  write_csv(x[order(x$param, x$dband, x$model), ], "sim_dense")
}

# ---------------------------------------------------------------------------
# Information at the true parameter (experiment-loglik, section 5)
# ---------------------------------------------------------------------------
ll <- file.path(repo, "experiment-loglik")
write_csv(
  utils::read.csv(file.path(ll, "calibration", "summary.csv")),
  "bridges"
)

# The `mutual_gwesp` figure by target density and sign of the clustering
# coefficient (theta_gwesp), from the per-sample export of the loglik
# experiment in data/loglik and data/samples (no CHPC needed). Median and
# 10th-90th percentiles of the loss over identified samples; the samples
# where the conditional model does not identify a coefficient are counted
# in info_gwesp_sign instead.
lfile <- file.path(repo, "data", "loglik", "mutual_gwesp.csv")
sfile <- file.path(repo, "data", "samples", "mutual_gwesp.csv")
if (file.exists(lfile) && file.exists(sfile)) {
  smp <- utils::read.csv(sfile)
  mg  <- merge(
    utils::read.csv(lfile),
    smp[c("M", "r", "ref_density", "theta_gwesp")],
    by = c("M", "r")
  )
  mg$identified <- as.logical(mg$identified)
  mg$clustering <- ifelse(mg$theta_gwesp > 0, "positive", "negative")
  qq <- function(x, p) stats::quantile(x, p, names = FALSE, na.rm = TRUE)
  sign_rows <- function(g) {
    l <- g$loss[g$identified]
    data.frame(
      param      = g$param[1],
      M          = g$M[1],
      clustering = g$clustering[1],
      samples    = nrow(g),
      not_id     = sum(!g$identified),
      med        = stats::median(l, na.rm = TRUE),
      p90        = qq(l, 0.9),
      over_5     = mean(l > 0.05, na.rm = TRUE)
    )
  }
  write_csv(
    do.call(rbind, lapply(
      split(mg, mg[c("param", "M", "clustering")], drop = TRUE),
      sign_rows
    )),
    "info_gwesp_sign"
  )
  cuts <- c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5)
  mg <- mg[mg$identified & is.finite(mg$loss), ]
  mg$dbin <- cut(mg$ref_density, cuts, include.lowest = TRUE)
  mids <- (head(cuts, -1) + cuts[-1]) / 2
  bins <- do.call(rbind, lapply(
    split(mg, mg[c("M", "dbin", "param", "clustering")], drop = TRUE),
    function(g) data.frame(
      M          = g$M[1],
      dmid       = mids[as.integer(g$dbin[1])],
      param      = g$param[1],
      clustering = g$clustering[1],
      samples    = nrow(g),
      med        = stats::median(g$loss),
      lo         = qq(g$loss, 0.1),
      hi         = qq(g$loss, 0.9)
    )
  ))
  write_csv(bins, "info_mutual_gwesp_bins")
}

if (!dir.exists(file.path(ll, "results-v3"))) {
  cat("no experiment-loglik/results-v3: info_loss not written\n")
  quit(save = "no")
}
old_wd <- setwd(ll)
source("likelihood.R")
band <- function(d) cut(d, c(0, 0.2, 0.35, 0.5))
read_design <- function(dir, design) {
  files <- list.files(dir, "^M.*\\.rds$", full.names = TRUE)
  Filter(function(z) identical(z$design_id, design), lapply(files, readRDS))
}
des <- do.call(rbind, lapply(exps, function(e) {
  smp <- read_design(
    file.path("results-v3", e),
    paste0(gsub("_", "-", e), "-population-v3")
  )
  do.call(rbind, lapply(smp, function(z) {
    nw <- z$networks
    if (!all(vapply(nw, function(v) identical(v$status, "ok"), TRUE)))
      return(NULL)
    ie <- sample_information_jk(nw, z$centre, "info_cond_exp", z$L)
    data.frame(
      design   = e,
      M        = z$M,
      ref_dens = z$ref_density,
      param    = if (is.null(z$pnames)) names(nw[[1]]$gamma) else z$pnames,
      loss     = 1 - unname(ie$info_kept),
      se       = unname(ie$se_info_kept),
      id       = unname(ie$identified)
    )
  }))
}))
setwd(old_wd)

q <- function(x, p) stats::quantile(x, p, names = FALSE, na.rm = TRUE)
info <- do.call(rbind, lapply(
  split(des, list(des$design, des$param, des$M), drop = TRUE),
  function(g) {
    i <- g$id
    data.frame(
      design        = g$design[1],
      param         = g$param[1],
      M             = g$M[1],
      samples       = nrow(g),
      not_id        = sum(!i),
      loss_median   = stats::median(g$loss[i], na.rm = TRUE),
      loss_p90      = q(g$loss[i], 0.9),
      se_median     = stats::median(g$se[i], na.rm = TRUE),
      signif_over_5 = mean((g$loss - 2 * g$se)[i] > 0.05, na.rm = TRUE)
    )
  }
))
write_csv(info[order(match(info$design, exps), info$param, info$M), ], "info_loss")

