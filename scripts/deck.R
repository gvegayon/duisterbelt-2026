# Tables and numbers of index.qmd, from the aggregate files in data/ (see
# data/README.md). Sourced by the setup chunk of index.qmd; every table on a
# slide comes from one of the functions below, so the deck has no
# hand-typed results.

dat <- function(name) {
  path <- file.path("data", paste0(name, ".csv"))
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, check.names = FALSE)
}

pct <- function(x, digits = 0) {
  ifelse(is.na(x), "--", paste0(formatC(100 * x, digits = digits, format = "f"), "%"))
}
num <- function(x, digits = 2) {
  x <- round(x, digits)
  x[x == 0] <- 0
  ifelse(is.na(x), "--", formatC(x, digits = digits, format = "f", big.mark = ","))
}
big <- function(x) formatC(x, format = "d", big.mark = ",")
md  <- function(x, align = NULL) {
  knitr::kable(x, format = "pipe", align = align, escape = FALSE, row.names = FALSE)
}

# A callout for a table whose data file has not been built yet.
pending <- function(name) {
  cat(
    "\n\n::: {.callout-warning}\n",
    "`data/", name, ".csv` is not built yet. Run `scripts/tables.R` on CHPC ",
    "(see `data/README.md`) and render again.\n:::\n\n",
    sep = ""
  )
}

labs_e <- c(
  mutual_gwesp              = "gwesp (directed)",
  mutual_gwdsp              = "gwdsp (directed)",
  mutual_gwesp_size         = "gwesp x size (directed)",
  undirected_gwesp          = "gwesp",
  undirected_gwesp_gwnsp    = "gwesp + gwnsp",
  undirected_gwdegree_gwesp = "gwdegree + gwesp",
  undirected_gwesp_curved   = "gwesp, decay estimated"
)

# ---------------------------------------------------------------------------
# Headline numbers (pooled row of sim_compare)
# ---------------------------------------------------------------------------
cmp     <- dat("sim_compare")
cmp_all <- cmp[cmp$group == "all", ]

# ---------------------------------------------------------------------------
# Why fits fail: ranges over M of three causes, for two models
# ---------------------------------------------------------------------------
tab_failures <- function(exps = c("mutual_gwesp", "undirected_gwdegree_gwesp")) {
  f <- dat("sim_failures_by_M")
  causes <- c(
    "MCMC did not mix" = "Chain did not mix",
    "non-finite SE"    = "Non-finite standard error",
    "timeout"          = "Timed out"
  )
  rng <- function(x) {
    r <- range(x)
    if (r[1] == r[2]) pct(r[1]) else paste0(pct(r[1]), "--", pct(r[2]))
  }
  rows <- do.call(rbind, lapply(exps, function(e) {
    x <- f[f$experiment == e, ]
    do.call(rbind, lapply(names(causes), function(k) {
      data.frame(
        Model         = if (k == names(causes)[1]) paste0("`", labs_e[[e]], "`") else "",
        Cause         = causes[[k]],
        Unconditional = rng(x[x$model == "uncond", k]),
        Conditional   = rng(x[x$model == "cond", k]),
        check.names   = FALSE
      )
    }))
  }))
  md(rows, c("l", "l", "r", "r"))
}

# ---------------------------------------------------------------------------
# Appendix tables
# ---------------------------------------------------------------------------
tab_dense <- function() {
  x <- dat("sim_dense")
  x <- x[x$dband %in% c("0.5–0.6", "0.7–0.8"), ]
  x <- x[order(x$param, x$dband, x$model != "uncond"), ]
  first <- !duplicated(x$param)
  firstd <- !duplicated(paste(x$param, x$dband))
  md(
    data.frame(
      Terms     = ifelse(first, ifelse(x$param == "GW terms", "Geometrically weighted", "Homophily and mutual"), ""),
      Density   = ifelse(firstd, gsub("–", "--", x$dband), ""),
      Model     = ifelse(x$model == "cond", "Conditional", "Unconditional"),
      Converged = pct(x$conv),
      `Median abs. error` = num(x$mae),
      Coverage  = pct(x$coverage),
      check.names = FALSE
    ),
    c("l", "l", "l", "r", "r", "r")
  )
}

tab_bridges <- function() {
  b <- dat("bridges")
  strata <- c(
    "gwesp > 0"    = "Positive `gwesp`",
    "gwesp < 0"    = "Negative `gwesp`",
    "ell_cond > 0" = "Negative `gwesp`, $\\hat{l}_{\\text{cond}} > 0$"
  )
  g <- function(s, e) b$abs_gap_median[b$stratum == s & b$effort == e]
  digits <- function(v) ifelse(v < 10, num(v, 2), num(v, 0))
  md(
    data.frame(
      `Networks (10 each)` = unname(strata),
      `Gap, effort $\\times 1$` = digits(sapply(names(strata), g, e = 8)),
      `$\\times 8$`  = digits(sapply(names(strata), g, e = 64)),
      `$\\times 64$` = digits(sapply(names(strata), g, e = 512)),
      check.names = FALSE
    ),
    c("l", "r", "r", "r")
  )
}
