# Figures of the simulation results, drawn from the aggregate tables in
# data/ (built by scripts/tables.R on CHPC, or by
# scripts/tables-from-reports.R from the two reports; see data/README.md).
# No fit or network is read here.
#
#   sim-by-m.png, sim-by-experiment.png   data/sim_compare.csv
#   sim-density.png                       data/sim_by_density.csv
#   info-loss.png                         data/info_loss.csv
#   info-mutual-gwesp.png                 data/info_mutual_gwesp_bins.csv
#                                         (only when tables.R has run)
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
measures <- c(
  conv = "Converged (%)",
  time = "Median fit time (s)",
  mae  = "Median |error| (GW terms)",
  cov  = "Coverage of 95% CI (%)"
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
  undirected_gwesp          = "gwesp",
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
# Information lost by conditioning (section 5 of experiment-loglik/report.md),
# median and 90th percentile over the identified samples.
# ---------------------------------------------------------------------------
info <- read.csv("data/info_loss.csv")
relabel <- c(
  nodematch.x = "nodematch",
  gwesp.decay = "gwesp decay",
  gwesp.logn  = "gwesp x log n"
)
hit <- info$param %in% names(relabel)
info$param[hit] <- relabel[info$param[hit]]

info$design <- factor(labs_e[info$design], levels = rev(labs_e))
info$param  <- factor(
  info$param,
  levels = c(
    "nodematch", "mutual", "gwesp", "gwesp decay", "gwesp x log n",
    "gwdegree", "gwdsp", "gwnsp"
  )
)
info$Mlab <- factor(
  paste("M =", info$M),
  levels = c("M = 1", "M = 5")
)
mcols <- c("M = 1" = "#7fb8bd", "M = 5" = accent)
dodge <- position_dodge(width = 0.6)

p_i <- ggplot(info, aes(y = design, colour = Mlab, shape = Mlab)) +
  geom_linerange(
    aes(xmin = loss_median, xmax = loss_p90),
    position  = dodge,
    linewidth = 1.3
  ) +
  geom_point(aes(x = loss_median), position = dodge, size = 3.4) +
  facet_grid(param ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = mcols) +
  scale_shape_manual(values = c("M = 1" = 17, "M = 5" = 16)) +
  scale_x_continuous(
    labels = function(x) paste0(round(100 * x), "%"),
    limits = c(0, 1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Information lost (point: median sample; line: to the 90th percentile)",
    y = NULL
  ) +
  theme_deck(19) +
  theme(
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle = 0, hjust = 1, face = "bold"),
    panel.spacing.y    = unit(0.25, "lines"),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = rel(0.85))
  )

save_fig(p_i, "info-loss.png", 12, 7.6)

# ---------------------------------------------------------------------------
# mutual_gwesp by target density (the info-figure of the loglik report).
# Needs data/info_mutual_gwesp_bins.csv, which only scripts/tables.R writes;
# without it, fig/info-mutual-gwesp.png is the report's own rendering.
# ---------------------------------------------------------------------------
if (file.exists("data/info_mutual_gwesp_bins.csv")) {
  bins <- read.csv("data/info_mutual_gwesp_bins.csv")
  bins$param <- factor(
    ifelse(bins$param == "nodematch.x", "nodematch", bins$param),
    c("nodematch", "mutual", "gwesp")
  )
  bins$Mlab <- paste("M =", bins$M)
  pcols <- c(nodematch = "#7fb8bd", mutual = muted, gwesp = accent)
  p_b <- ggplot(bins, aes(dmid, med, colour = param, fill = param)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.4) +
    facet_wrap(~Mlab) +
    scale_colour_manual(values = pcols) +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    labs(x = "Target density", y = "Information lost by conditioning") +
    theme_deck()
  save_fig(p_b, "info-mutual-gwesp.png", 10, 5.2)
}
