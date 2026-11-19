# Rebuilds, without the CHPC results, the files of data/ that the two
# reports of the research repository already print, by reading the Markdown
# tables of
#
#   report/report.md                 (rendered from report/report.qmd)
#   experiment-loglik/report.md      (rendered from experiment-loglik/report.qmd)
#   experiment-loglik/calibration/summary.csv
#
# The files have the same columns as those of scripts/tables.R, which is the
# authoritative version: it computes them from the fits and adds the tables
# no report prints (sim_levels, sim_paired, sim_discordant_causes,
# info_mutual_gwesp_bins). Values read from the reports carry their rounding
# (shares to whole percentages, errors to two decimals).
#
# Run from the deck's root:
#   Rscript scripts/tables-from-reports.R [--repo=<repo>] [--out=data]

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

# Every pipe table of a Markdown file, named by the caption that follows it
# ("Table 2", "Table 3c", ...) or, without one, by the heading above it plus
# its position under that heading.
read_md_tables <- function(path) {
  x <- readLines(path, warn = FALSE)
  is_tab <- startsWith(x, "|")
  runs <- rle(is_tab)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  out <- list()
  heading <- ""
  count <- 0L
  for (k in seq_along(runs$values)) {
    if (!runs$values[k]) {
      h <- grep("^#+ ", x[starts[k]:ends[k]], value = TRUE)
      if (length(h)) {
        heading <- sub("^#+ ", "", h[length(h)])
        count <- 0L
      }
      next
    }
    rows <- x[starts[k]:ends[k]]
    cells <- lapply(strsplit(rows, "|", fixed = TRUE), function(r) {
      trimws(r[-1])
    })
    df <- as.data.frame(
      do.call(rbind, cells[-(1:2)]),
      stringsAsFactors = FALSE
    )
    names(df) <- cells[[1]]
    after <- x[seq(ends[k] + 1L, min(ends[k] + 3L, length(x)))]
    cap <- regmatches(after, regexpr("^\\*\\*Table [0-9A-Za-z]+", after))
    count <- count + 1L
    name <- if (length(cap)) {
      sub("^\\*\\*", "", cap[1])
    } else {
      paste0(heading, " #", count)
    }
    out[[name]] <- df
  }
  out
}

# "72%" -> 0.72, "1,899" -> 1899, "<0.001" -> 0.001, "—" -> NA.
num <- function(v) {
  v <- gsub("[,%<]|\\\\", "", v)
  v[v %in% c("—", "")] <- NA
  suppressWarnings(as.numeric(v))
}
pct <- function(v) num(v) / 100

sim <- read_md_tables(file.path(repo, "report", "report.md"))
ll  <- read_md_tables(file.path(repo, "experiment-loglik", "report.md"))

exps <- c(
  "mutual_gwesp",
  "mutual_gwdsp",
  "mutual_gwesp_size",
  "undirected_gwesp",
  "undirected_gwesp_gwnsp",
  "undirected_gwdegree_gwesp",
  "undirected_gwesp_curved"
)

# Table 2: conditional vs unconditional.
t2 <- sim[["Table 2"]]
write_csv(
  data.frame(
    group       = sub("^all, M = ", "M", t2$Group),
    samples     = num(t2$Samples),
    conv_cond   = pct(t2$`Conv. cond`),
    conv_uncond = pct(t2$`Conv. uncond`),
    conv_p      = num(t2$p),
    time_cond   = num(t2$`Time cond`),
    time_uncond = num(t2$`Time uncond`),
    bias_cond   = num(t2$`Bias cond`),
    bias_uncond = num(t2$`Bias uncond`),
    mae_cond    = num(t2$`MAE cond`),
    mae_uncond  = num(t2$`MAE uncond`),
    cov_cond    = pct(t2$`Cov. cond`),
    cov_uncond  = pct(t2$`Cov. uncond`)
  ),
  "sim_compare"
)

# Tables 3c-9c (by density) and 3b-9b (failures by M), experiment k + 2.
dens <- fail <- list()
for (k in seq_along(exps)) {
  d <- sim[[paste0("Table ", k + 2, "c")]]
  dens[[k]] <- data.frame(
    experiment = exps[k],
    dband      = d$Density,
    param      = d$Parameter,
    model      = d$Model,
    fits       = num(d$Fits),
    conv       = pct(d$Converged),
    time       = num(d$`Time (s)`),
    bias       = num(d$Bias),
    mae        = num(d$MAE),
    coverage   = pct(d$Coverage),
    sign       = NA_real_
  )
  f <- sim[[paste0("Table ", k + 2, "b")]]
  fail[[k]] <- data.frame(
    experiment = exps[k],
    M          = num(f$M),
    model      = f$Model,
    fits       = num(f$Fits),
    failed     = pct(f$Failed),
    f[c("timeout", "non-finite SE", "MCMC did not mix", "ergm d2e bug",
        "other numerical error")],
    check.names = FALSE
  )
  for (j in 6:10) fail[[k]][[j]] <- pct(fail[[k]][[j]])
}
write_csv(do.call(rbind, dens), "sim_by_density")
write_csv(do.call(rbind, fail), "sim_failures_by_M")

# Table A1: densities above 0.5 (earlier design).
a1 <- sim[["Table A1"]]
write_csv(
  data.frame(
    dband    = a1$Density,
    param    = a1$Terms,
    model    = a1$Model,
    fits     = num(a1$Fits),
    conv     = pct(a1$Converged),
    time     = NA_real_,
    bias     = num(a1$Bias),
    mae      = num(a1$MAE),
    coverage = pct(a1$Coverage),
    sign     = pct(a1$`Right sign`)
  ),
  "sim_dense"
)

# Information lost, all designs (loglik report, section 5: the table with a
# `loss_p90` column and a `design` column).
it <- Filter(function(d) all(c("design", "loss_p90") %in% names(d)), ll)[[1]]
write_csv(
  data.frame(
    design        = it$design,
    param         = it$param,
    M             = num(it$M),
    samples       = num(it$samples),
    not_id        = num(it$not_id),
    loss_median   = num(it$loss_median),
    loss_p90      = num(it$loss_p90),
    se_median     = num(it$se_median),
    signif_over_5 = num(it$signif_over_5)
  ),
  "info_loss"
)

# Conditional bridges: the calibration summary, as is.
write_csv(
  utils::read.csv(file.path(repo, "experiment-loglik", "calibration", "summary.csv")),
  "bridges"
)
