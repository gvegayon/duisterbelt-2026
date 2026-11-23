# Figures of the simulation results, drawn from the aggregate tables in
# data/ (built by scripts/tables.R on CHPC, or by
# scripts/tables-from-reports.R from the two reports; see data/README.md).
# The two info-* figures plot one point per sample, from the research repo's
# per-sample export (see "Information lost by conditioning" below). No fit
# or network is read here.
#
#   sim-by-m.png, sim-by-experiment.png   data/sim_compare.csv
#   sim-density.png                       data/sim_by_density.csv
#   info-terms.png                        research repo data/loglik, data/samples
#   info-mutual-gwesp.png                 research repo data/loglik, data/samples
#
# Run from the deck's root: Rscript scripts/figures.R

library(ggplot2)

accent <- "#00707a"
muted  <- "#8a8f98"
cols   <- c(Conditional = accent, Unconditional = muted)

theme_deck <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "#e5e7eb", linewidth = 0.3),
      strip.text         = element_text(face = "bold", hjust = 0, size = rel(0.95)),
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.margin      = margin(0, 0, 0, 0),
      plot.title.position = "plot",
      axis.title         = element_text(colour = "#4b5563", size = rel(0.85)),
      axis.text          = element_text(colour = "#4b5563")
    )
}

save_fig <- function(p, file, width, height) {
  ggsave(
    file.path("fig", file),
    p,
    width  = width,
    height = height,
    dpi    = 200,
    bg     = "white"
  )
}

# ---------------------------------------------------------------------------
# Conditional vs unconditional (Table 2 of report/report.md)
# ---------------------------------------------------------------------------
tab2 <- read.csv("data/sim_compare.csv")
tab2 <- tab2[tab2$group != "all", ]
# Shares as percentages, as the panel titles say.
for (v in c("conv_cond", "conv_uncond", "cov_cond", "cov_uncond"))
  tab2[[v]] <- 100 * tab2[[v]]

# Long format: one row per (group, measure, model).
# Time is that of the converged fits (time_ok_*): the median over all fits
# counts each timeout at the one-hour limit, which measures how often a model
# times out rather than how fast it fits.
measures <- c(
  conv    = "Converged (%)",
  time_ok = "Time, converged fits (s)",
  mae     = "Median |error| (GW terms)",
  cov     = "Coverage of 95% CI (%)"
)

long <- do.call(rbind, lapply(names(measures), function(m) {
  rbind(
    data.frame(
      group   = tab2$group,
      measure = measures[[m]],
      model   = "Conditional",
      value   = tab2[[paste0(m, "_cond")]]
    ),
    data.frame(
      group   = tab2$group,
      measure = measures[[m]],
      model   = "Unconditional",
      value   = tab2[[paste0(m, "_uncond")]]
    )
  )
}))
long$measure <- factor(long$measure, levels = measures)
long$model   <- factor(long$model, levels = names(cols))

# --- By number of networks --------------------------------------------------
by_m   <- long[grepl("^M[0-9]+$", long$group), ]
by_m$M <- as.integer(sub("M", "", by_m$group))

# Reference line for the nominal coverage, and y-ranges that start where the
# comparison is honest (0 for magnitudes).
ref <- data.frame(
  measure = factor(measures[["cov"]], levels = measures),
  y       = 95
)
lims <- data.frame(
  measure = factor(rep(measures, each = 2), levels = measures),
  M       = 1,
  value   = c(0, 100, 0, 250, 0, 0.4, 80, 100)
)

p_m <- ggplot(by_m, aes(M, value, colour = model, shape = model)) +
  geom_blank(data = lims, aes(M, value), inherit.aes = FALSE) +
  geom_hline(
    data      = ref,
    aes(yintercept = y),
    linetype  = "dashed",
    colour    = "#9ca3af",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~measure, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = c(Conditional = 16, Unconditional = 17)) +
  scale_x_continuous(breaks = c(1, 5, 10, 20)) +
  labs(x = "Networks per sample (M)", y = NULL) +
  theme_deck()

save_fig(p_m, "sim-by-m.png", 13, 4.2)

# --- By experiment (dumbbells) ----------------------------------------------
by_e <- long[!grepl("^M[0-9]+$", long$group), ]
labs_e <- c(
  mutual_gwesp              = "gwesp (directed)",
  mutual_gwdsp              = "gwdsp (directed)",
  mutual_gwesp_size         = "gwesp x size (directed)",
  undirected_gwesp          = "Base model",
  undirected_gwesp_gwnsp    = "gwesp + gwnsp",
  undirected_gwdegree_gwesp = "gwdegree + gwesp",
  undirected_gwesp_curved   = "gwesp, decay estimated"
)
by_e$label <- factor(labs_e[by_e$group], levels = rev(labs_e))
by_e <- by_e[by_e$measure != measures[["mae"]], ]
by_e$measure <- droplevels(by_e$measure)

seg <- reshape(
  by_e[, c("label", "measure", "model", "value")],
  idvar     = c("label", "measure"),
  timevar   = "model",
  direction = "wide"
)

p_e <- ggplot(by_e, aes(value, label)) +
  geom_segment(
    data      = seg,
    aes(
      x    = value.Unconditional,
      xend = value.Conditional,
      y    = label,
      yend = label
    ),
    colour    = "#c7cbd1",
    linewidth = 1.2
  ) +
  geom_point(aes(colour = model, shape = model), size = 3.4) +
  facet_wrap(~measure, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = c(Conditional = 16, Unconditional = 17)) +
  labs(x = NULL, y = NULL) +
  theme_deck()

save_fig(p_e, "sim-by-experiment.png", 13, 4.4)

# ---------------------------------------------------------------------------
# mutual_gwesp by observed density (Table 3c of report/report.md), bins with
# at least 40 fits per model.
# ---------------------------------------------------------------------------
dens <- read.csv("data/sim_by_density.csv")
dens <- dens[dens$experiment == "mutual_gwesp", ]
# Bins with at least 40 fits in each model.
keep <- tapply(dens$fits, dens$dband, min) >= 40
dens <- dens[dens$dband %in% names(keep)[keep], ]
dens <- data.frame(
  density     = dens$dband[dens$model == "cond"],
  conv_cond   = 100 * dens$conv[dens$model == "cond"],
  conv_uncond = 100 * dens$conv[dens$model == "uncond"],
  mae_cond    = dens$mae[dens$model == "cond"],
  mae_uncond  = dens$mae[dens$model == "uncond"]
)
dens$density <- gsub("\u2013", "-", dens$density)

dens_long <- rbind(
  data.frame(
    density = dens$density,
    measure = "Converged (%)",
    model   = "Conditional",
    value   = dens$conv_cond
  ),
  data.frame(
    density = dens$density,
    measure = "Converged (%)",
    model   = "Unconditional",
    value   = dens$conv_uncond
  ),
  data.frame(
    density = dens$density,
    measure = "Median |error|, gwesp",
    model   = "Conditional",
    value   = dens$mae_cond
  ),
  data.frame(
    density = dens$density,
    measure = "Median |error|, gwesp",
    model   = "Unconditional",
    value   = dens$mae_uncond
  )
)
dens_long$density <- factor(dens_long$density, levels = dens$density)
dens_long$model   <- factor(dens_long$model, levels = names(cols))

p_d <- ggplot(
  dens_long,
  aes(density, value, colour = model, shape = model, group = model)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~measure, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = c(Conditional = 16, Unconditional = 17)) +
  expand_limits(y = 0) +
  labs(x = "Observed mean density of the sample", y = NULL) +
  theme_deck() +
  theme(axis.text.x = element_text(size = rel(0.8)))

save_fig(p_d, "sim-density.png", 10, 4.2)

# ---------------------------------------------------------------------------
# Information lost by conditioning, per sample. Both figures below read the
# per-sample export of the research repo (data/loglik, data/samples) in
# place; nothing is copied into the deck. Jackknife noise makes a few losses
# slightly negative: they are drawn at 0. Non-identified samples are left
# out of the curves.
# ---------------------------------------------------------------------------
repo <- "../../research/ergm-cond-consistency"
read_loglik <- function(design) {
  lfile <- file.path(repo, "data", "loglik", paste0(design, ".csv"))
  sfile <- file.path(repo, "data", "samples", paste0(design, ".csv"))
  if (!file.exists(lfile) || !file.exists(sfile)) return(NULL)
  smp <- read.csv(sfile)
  x   <- merge(
    read.csv(lfile),
    smp[c("M", "r", "ref_density", "theta_gwesp")],
    by = c("M", "r")
  )
  x$identified <- as.logical(x$identified)
  x$clustering <- ifelse(x$theta_gwesp > 0, "positive", "negative")
  x$Mlab       <- factor(paste("M =", x$M), c("M = 1", "M = 5"))
  x
}
loss_cols <- c("M = 1" = "#e08a6a", "M = 5" = "#8c2f12")
scale_loss <- function(...) {
  scale_x_continuous(
    trans  = scales::pseudo_log_trans(sigma = 0.001, base = 10),
    breaks = c(0.001, 0.01, 0.05, 0.2, 1),
    labels = c("0.1%", "1%", "5%", "20%", "100%"),
    ...
  )
}
ridges <- function(
    point_size  = 1,
    height      = 0.22,
    scale       = 0.75,
    point_alpha = 0.45
) {
  ggridges::geom_density_ridges(
    jittered_points = TRUE,
    position        = ggridges::position_raincloud(height = height, ygap = 0.04),
    point_size      = point_size,
    point_alpha     = point_alpha,
    alpha           = 0.35,
    scale           = scale,
    rel_min_height  = 0.005,
    bandwidth       = 0.12
  )
}

# "But it depends on the term": three undirected models that differ by one
# term. Strips name the model; rows are its coefficients, by M.
terms_models <- c(
  undirected_gwesp          = "Model:\ngwesp",
  undirected_gwdegree_gwesp = "Model:\ngwdegree + gwesp",
  undirected_gwesp_gwnsp    = "Model:\ngwesp + gwnsp"
)
tm <- do.call(rbind, lapply(names(terms_models), function(e) {
  x <- read_loglik(e)
  if (is.null(x)) return(NULL)
  x[x$identified, c("experiment", "param", "Mlab", "loss")]
}))
if (!is.null(tm)) {
  tm$model <- factor(terms_models[tm$experiment], terms_models)
  tm$param <- factor(
    ifelse(tm$param == "nodematch.x", "nodematch", tm$param),
    rev(c("gwnsp", "gwdegree", "gwesp", "nodematch"))
  )
  tm$loss <- pmax(tm$loss, 0)
  p_t <- ggplot(tm, aes(loss, param, fill = Mlab, colour = Mlab)) +
    geom_vline(xintercept = 0.05, colour = "#9ca3af", linetype = "dashed") +
    ridges(point_size = 0.7, height = 0.2, scale = 1.1, point_alpha = 0.3) +
    facet_grid(model ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_loss() +
    coord_cartesian(xlim = c(0, 1)) +
    scale_fill_manual(values = loss_cols) +
    scale_colour_manual(values = loss_cols) +
    labs(x = "Information lost by conditioning (log scale)", y = NULL) +
    theme_deck() +
    theme(
      strip.placement    = "outside",
      strip.text.y.left  = element_text(angle = 0, hjust = 1, face = "bold"),
      panel.spacing.y    = unit(0.8, "lines"),
      panel.grid.major.y = element_blank()
    )
  save_fig(p_t, "info-terms.png", 8, 5.6)
}

# ---------------------------------------------------------------------------
# gwesp in mutual_gwesp: the information lost in every sample, by target
# density and sign of the clustering coefficient (ridgelines of the
# distribution, with the samples as points below). Non-identified samples are
# counted in the strips.
# ---------------------------------------------------------------------------
g <- read_loglik("mutual_gwesp")
if (!is.null(g)) {
  g <- g[g$param == "gwesp", ]
  nonid <- tapply(!g$identified, g$clustering, mean)
  strip <- c(
    positive = "Positive clustering",
    negative = sprintf(
      "Negative (%s not identified)",
      paste0(round(100 * nonid[["negative"]]), "%")
    )
  )
  g <- g[g$identified, ]
  g$panel <- factor(strip[g$clustering], strip)
  g$dbin  <- cut(
    g$ref_density,
    c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5),
    labels         = c("0.05–0.10", "0.10–0.15", "0.15–0.20", "0.20–0.30", "0.30–0.50"),
    include.lowest = TRUE
  )
  g$loss <- pmax(g$loss, 0)
  p_b <- ggplot(g, aes(loss, dbin, fill = Mlab, colour = Mlab)) +
    geom_vline(xintercept = 0.05, colour = "#9ca3af", linetype = "dashed") +
    ridges() +
    facet_wrap(~panel) +
    scale_loss() +
    coord_cartesian(xlim = c(0, 1)) +
    scale_fill_manual(values = loss_cols) +
    scale_colour_manual(values = loss_cols) +
    labs(x = "Information on gwesp lost (log scale)", y = "Target density") +
    theme_deck() +
    theme(panel.grid.major.y = element_blank())
  save_fig(p_b, "info-mutual-gwesp.png", 7, 4.3)
}

# ---------------------------------------------------------------------------
# Paired outcomes of the two fits of each sample, positive clustering, by
# observed mean density and M (data/sim_paired_density_sign.csv; the
# all-samples version is report/cond-failures.md, Figure 1). Same outcome
# colours as that figure.
# ---------------------------------------------------------------------------
if (file.exists("data/sim_paired_density_sign.csv")) {
  pd <- read.csv("data/sim_paired_density_sign.csv", check.names = FALSE)
  pd <- pd[pd$clustering == "positive", ]
  outcomes <- c(
    both        = "Both converge",
    only_cond   = "Conditional only",
    only_uncond = "Full only",
    neither     = "Neither"
  )
  outcome_cols <- c(
    "Both converge"    = "#2a78d6",
    "Conditional only" = "#eb6834",
    "Full only"        = "#1baf7a",
    "Neither"          = "#eda100"
  )
  long <- do.call(rbind, lapply(names(outcomes), function(k) data.frame(
    M       = pd$M,
    dband   = pd$dband,
    samples = pd$samples,
    outcome = outcomes[[k]],
    share   = pd[[k]]
  )))
  long$outcome <- factor(long$outcome, rev(unname(outcomes)))
  long$dband <- factor(long$dband, c("0–0.1", "0.1–0.2", "0.2–0.3", "0.3–0.4", "0.4–0.5", "0.5+"))
  long$Mlab <- factor(paste("M =", long$M), paste("M =", sort(unique(long$M))))
  # Bins with fewer than 30 samples are too noisy to show.
  long <- long[long$samples >= 30, ]
  p_po <- ggplot(long, aes(dband, share, fill = outcome)) +
    geom_col(width = 0.78, colour = "white", linewidth = 0.4) +
    geom_text(
      aes(
        label  = ifelse(share >= 0.08, round(100 * share), ""),
        colour = ifelse(outcome == "Both converge", "white", "#111827")
      ),
      position = position_stack(vjust = 0.5),
      size     = 3
    ) +
    scale_colour_identity() +
    facet_wrap(~Mlab, nrow = 1) +
    scale_fill_manual(values = outcome_cols, breaks = unname(outcomes)) +
    scale_y_continuous(labels = function(v) paste0(100 * v, "%"), expand = c(0, 0)) +
    labs(x = "Observed mean density", y = "Share of samples") +
    theme_deck(13) +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    )
  save_fig(p_po, "sim-paired-positive.png", 8, 3.8)
}
