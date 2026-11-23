# Writes data/info_by_design.csv: the information lost by conditioning on
# the edge count, for each GW coefficient of each model design, from the
# per-sample export of the loglik experiment in the research repository
# (data/loglik/ and data/samples/; no CHPC needed). M = 1 and 5 pooled.
#
#   Rscript scripts/info-by-design.R --repo=<research repo> --out=data
#
# Also sourced by scripts/tables.R. Only aggregates are written: medians,
# their 95% confidence intervals and counts over samples.
#
# The median and its CI are over the samples where the conditional model
# identifies the coefficient; the CI uses binomial order statistics
# (distribution free). The sign of a coefficient is that of its own drawn
# value (theta_<param>). Not identified: the share of samples where the
# coefficient has zero efficient information under conditioning.

if (!exists("repo")) {
  args <- commandArgs(trailingOnly = TRUE)
  arg  <- function(name, default) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
  }
  repo <- normalizePath(arg("repo", "../../research/ergm-cond-consistency"))
  out  <- arg("out", "data")
}

designs <- c(
  "mutual_gwesp",
  "mutual_gwdsp",
  "mutual_gwesp_size",
  "undirected_gwesp",
  "undirected_gwesp_gwnsp",
  "undirected_gwdegree_gwesp",
  "undirected_gwesp_curved"
)

# Median of x and its 95% CI from order statistics.
median_ci <- function(x) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 10) return(c(med = NA, lo = NA, hi = NA))
  lo <- stats::qbinom(0.025, n, 0.5)
  hi <- stats::qbinom(0.975, n, 0.5) + 1
  c(med = stats::median(x), lo = x[max(lo, 1)], hi = x[min(hi, n)])
}

stats_of <- function(g) {
  l <- g$loss[g$identified]
  m <- median_ci(l)
  data.frame(
    samples = nrow(g),
    not_id  = mean(!g$identified),
    med     = m[["med"]],
    lo      = m[["lo"]],
    hi      = m[["hi"]]
  )
}

rows <- list()
for (e in designs) {
  lfile <- file.path(repo, "data", "loglik", paste0(e, ".csv"))
  sfile <- file.path(repo, "data", "samples", paste0(e, ".csv"))
  if (!file.exists(lfile) || !file.exists(sfile)) next
  ll  <- utils::read.csv(lfile)
  smp <- utils::read.csv(sfile)
  ll$identified <- as.logical(ll$identified)
  # GW main effects only (not the fixed size interaction or the decay).
  gw <- grep("^gw[a-z]+$", unique(ll$param), value = TRUE)
  for (p in gw) {
    x <- merge(
      ll[ll$param == p, c("M", "r", "loss", "identified")],
      smp[c("M", "r", paste0("theta_", p))],
      by = c("M", "r")
    )
    pos <- x[[paste0("theta_", p)]] > 0
    a  <- stats_of(x)
    sp <- stats_of(x[pos, ])
    sn <- stats_of(x[!pos, ])
    names(sp) <- paste0(names(sp), "_pos")
    names(sn) <- paste0(names(sn), "_neg")
    rows[[length(rows) + 1]] <- cbind(
      data.frame(design = e, param = p),
      a,
      sp,
      sn
    )
  }
}
info_by_design <- do.call(rbind, rows)
utils::write.csv(
  info_by_design,
  file.path(out, "info_by_design.csv"),
  row.names = FALSE
)
cat("wrote", file.path(out, "info_by_design.csv"), "\n")
