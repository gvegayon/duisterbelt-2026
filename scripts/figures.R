# Figures for the deck, drawn from the AGGREGATE tables of the two reports in
# research/ergm-cond-consistency (no fits or networks are read here):
#
#   - report/report.md, Table 2 (conditional vs unconditional MCMLE) and
#     Table 3c (mutual_gwesp by density);
#   - experiment-loglik/report.md, section 5 (information lost, all designs).
#
# The numbers are transcribed by hand, so re-check them against the reports if
# those are re-rendered. Run from the deck's root: Rscript scripts/figures.R

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
# Table 2 of report/report.md
# ---------------------------------------------------------------------------
tab2 <- read.csv(text = "
group,samples,conv_cond,conv_uncond,time_cond,time_uncond,bias_cond,bias_uncond,mae_cond,mae_uncond,cov_cond,cov_uncond
mutual_gwesp,1899,72,67,33,72,0.01,0.00,0.12,0.13,94,93
mutual_gwdsp,1300,61,55,41,55,0.01,0.02,0.13,0.15,87,84
mutual_gwesp_size,1186,82,78,21,48,-0.01,-0.01,0.15,0.13,94,93
undirected_gwesp,1983,66,63,11,32,0.03,0.03,0.14,0.14,94,94
undirected_gwesp_gwnsp,1111,52,47,160,187,0.00,0.02,0.14,0.16,92,81
undirected_gwdegree_gwesp,817,55,42,20,28,0.05,0.07,0.21,0.22,95,96
undirected_gwesp_curved,1883,53,41,55,97,0.00,-0.01,0.24,0.26,89,89
M1,2361,50,43,4,5,0.06,0.07,0.34,0.35,94,91
M5,2440,64,55,20,40,0.01,0.02,0.18,0.18,92,91
M10,2625,66,59,48,100,0.01,0.00,0.13,0.13,92,89
M20,2753,66,59,120,240,0.01,0.01,0.10,0.11,92,89
", strip.white = TRUE)

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
# Table 3c of report/report.md: mutual_gwesp by observed density (bins with
# at least 40 fits per model).
# ---------------------------------------------------------------------------
dens <- read.csv(text = "
density,conv_cond,conv_uncond,mae_cond,mae_uncond
0-0.05,54,40,0.11,0.11
0.05-0.1,94,84,0.08,0.08
0.1-0.2,88,77,0.06,0.08
0.2-0.3,69,65,0.08,0.08
0.3-0.4,61,59,0.14,0.14
0.4-0.5,70,67,0.43,0.41
0.5-0.6,93,91,0.79,0.75
", strip.white = TRUE)

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
# Section 5 of experiment-loglik/report.md: information lost by conditioning
# (expected version), median and 90th percentile over identified samples.
# ---------------------------------------------------------------------------
info <- read.csv(text = "
design,param,M,loss_median,loss_p90
mutual_gwesp,gwesp,1,0.003,0.079
mutual_gwesp,gwesp,5,0.005,0.163
mutual_gwesp,mutual,1,0.001,0.007
mutual_gwesp,mutual,5,0.002,0.018
mutual_gwesp,nodematch,1,0.001,0.013
mutual_gwesp,nodematch,5,0.003,0.050
mutual_gwdsp,gwdsp,1,0.006,0.152
mutual_gwdsp,gwdsp,5,0.014,0.702
mutual_gwdsp,mutual,1,0.001,0.048
mutual_gwdsp,mutual,5,0.002,0.461
mutual_gwdsp,nodematch,1,0.001,0.016
mutual_gwdsp,nodematch,5,0.003,0.076
mutual_gwesp_size,gwesp,1,0.005,0.150
mutual_gwesp_size,gwesp,5,0.048,0.767
mutual_gwesp_size,gwesp x log n,5,0.068,0.936
mutual_gwesp_size,mutual,1,0.002,0.023
mutual_gwesp_size,mutual,5,0.006,0.055
mutual_gwesp_size,nodematch,1,0.002,0.026
mutual_gwesp_size,nodematch,5,0.011,0.139
undirected_gwesp,gwesp,1,0.002,0.057
undirected_gwesp,gwesp,5,0.003,0.060
undirected_gwesp,nodematch,1,0.001,0.007
undirected_gwesp,nodematch,5,0.002,0.030
undirected_gwesp_gwnsp,gwesp,1,0.017,0.291
undirected_gwesp_gwnsp,gwesp,5,0.068,0.797
undirected_gwesp_gwnsp,gwnsp,1,0.025,0.732
undirected_gwesp_gwnsp,gwnsp,5,0.306,0.921
undirected_gwesp_gwnsp,nodematch,1,0.002,0.255
undirected_gwesp_gwnsp,nodematch,5,0.025,0.490
undirected_gwdegree_gwesp,gwdegree,1,0.020,0.212
undirected_gwdegree_gwesp,gwdegree,5,0.032,0.382
undirected_gwdegree_gwesp,gwesp,1,0.003,0.043
undirected_gwdegree_gwesp,gwesp,5,0.002,0.054
undirected_gwdegree_gwesp,nodematch,1,0.001,0.008
undirected_gwdegree_gwesp,nodematch,5,0.002,0.029
undirected_gwesp_curved,gwesp,1,0.002,0.060
undirected_gwesp_curved,gwesp,5,0.003,0.078
undirected_gwesp_curved,gwesp decay,1,0.003,0.089
undirected_gwesp_curved,gwesp decay,5,0.003,0.140
undirected_gwesp_curved,nodematch,1,0.001,0.010
undirected_gwesp_curved,nodematch,5,0.002,0.031
", strip.white = TRUE)

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
