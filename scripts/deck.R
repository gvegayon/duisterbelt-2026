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
# One row of sim_compare: an experiment, "M1".."M20" or "all".
cg <- function(g) cmp[cmp$group == g, ]
# Convergence gain of the conditional model, percentage points.
gain <- function(g) round(100 * (cg(g)$conv_cond - cg(g)$conv_uncond))

# Paired outcomes, pooled (sim_paired, `finite` = the report's convergence).
paired     <- dat("sim_paired")
paired_all <- paired[paired$definition == "finite" & paired$by == "all", ]
# One row of sim_paired: definition "finite" (converged) or "usable", group
# "positive", "negative" or "all".
prow <- function(definition, group) {
  paired[paired$definition == definition & paired$group == group &
           paired$by %in% c("clustering", "all"), ]
}

# Rescue rates by clustering sign: how often each model works when the other
# does not, for convergence and for usable GW estimates (report
# cond-failures, Table 1).
tab_rescue <- function() {
  if (is.null(prow("usable", "all")) || !nrow(prow("usable", "all")))
    return(pending("sim_paired"))
  groups <- c(positive = "Positive", negative = "Negative", all = "All")
  cell <- function(d, g) {
    x <- prow(d, g)
    paste0("**", pct(x$cond_given_uncond_failed), "** vs ", pct(x$uncond_given_cond_failed))
  }
  md(data.frame(
    Clustering  = groups,
    Converges   = vapply(names(groups), cell, "", d = "finite"),
    `Usable`    = vapply(names(groups), cell, "", d = "usable"),
    check.names = FALSE
  ), align = "lcc")
}

# Share of fits that time out, by experiment and model, over all M.
timeout_share <- function(e, m) {
  f <- dat("sim_failures_by_M")
  f <- f[f$experiment == e & f$model == m, ]
  sum(f$timeout * f$fits) / sum(f$fits)
}

# Headline comparison by clustering sign (sim_compare_sign;
# scripts/sim-by-sign.R), and the paired rows of one clustering sign.
csign <- dat("sim_compare_sign")
crow  <- function(clustering) csign[csign$clustering == clustering, ]
psign <- function(clustering) {
  paired[paired$definition == "finite" & paired$by == "clustering" &
           paired$group == clustering, ]
}

# Quality of the GW coefficients by clustering sign (sim_quality).
quality <- dat("sim_quality")
qrow <- function(clustering, M = "all") {
  quality[quality$clustering == clustering & quality$M == M, ]
}
tab_quality <- function() {
  if (is.null(quality)) return(pending("sim_quality"))
  vs <- function(a, b) paste0("**", pct(a), "** vs ", pct(b))
  p <- qrow("positive")
  n <- qrow("negative")
  md(data.frame(
    ` `                     = c("Converged", "Usable (SE ≤ 1)", "Accurate (covers truth, excludes 0)"),
    `Positive clustering`   = c(vs(p$conv_cond, p$conv_uncond), vs(p$use_cond, p$use_uncond), vs(p$acc_cond, p$acc_uncond)),
    `Negative clustering`   = c(vs(n$conv_cond, n$conv_uncond), vs(n$use_cond, n$use_uncond), vs(n$acc_cond, n$acc_uncond)),
    check.names = FALSE
  ), align = "lcc")
}

# Information lost on one coefficient (info_loss): median or 90th
# percentile over identified samples.
info_loss <- dat("info_loss")
iloss <- function(design, param, M, stat = "loss_median") {
  info_loss[info_loss$design == design & info_loss$param == param &
              info_loss$M == M, stat]
}

# Information lost on a coefficient of mutual_gwesp by clustering sign
# (info_gwesp_sign), and the binned figure data (info_mutual_gwesp_bins).
isign <- dat("info_gwesp_sign")
ibins <- dat("info_mutual_gwesp_bins")
irow  <- function(M, clustering, param = "gwesp") {
  isign[isign$param == param & isign$M == M & isign$clustering == clustering, ]
}
# Information lost on each GW coefficient by model design and sign of the
# coefficient (info_by_design; scripts/info-by-design.R), M = 1 and 5.
idesign <- dat("info_by_design")
design_label <- c(
  mutual_gwesp              = "Directed: mutual + gwesp",
  mutual_gwdsp              = "Directed: mutual + gwdsp",
  mutual_gwesp_size         = "Directed: mutual + gwesp × size",
  undirected_gwesp          = "Undirected: gwesp",
  undirected_gwesp_gwnsp    = "Undirected: gwesp + gwnsp",
  undirected_gwdegree_gwesp = "Undirected: gwdegree + gwesp",
  undirected_gwesp_curved   = "Undirected: curved gwesp"
)
tab_info_design <- function() {
  if (is.null(idesign)) return(pending("info_by_design"))
  x <- idesign
  p1 <- function(v) ifelse(!is.na(v) & v < 0.0005, "<0.1%", pct(v, 1))
  signed <- function(med, nid) {
    paste0("**", p1(med), "**", ifelse(nid >= 0.005, paste0(" (", pct(nid), " n.i.)"), ""))
  }
  lab <- design_label[x$design]
  lab[duplicated(x$design)] <- ""
  md(data.frame(
    Model                  = lab,
    Term                   = paste0("`", x$param, "`"),
    `Median [95% CI]`      = paste0("**", p1(x$med), "** [", p1(x$lo), ", ", p1(x$hi), "]"),
    `Positive coefficient` = signed(x$med_pos, x$not_id_pos),
    `Negative coefficient` = signed(x$med_neg, x$not_id_neg),
    check.names            = FALSE
  ), align = "llccc")
}

# Share of samples where the conditional model does not identify gwesp,
# pooled over M unless one is given.
inonid <- function(clustering, M = c(1, 5)) {
  x <- do.call(rbind, lapply(M, irow, clustering = clustering))
  sum(x$not_id) / sum(x$samples)
}

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
