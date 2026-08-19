## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----data---------------------------------------------------------------------
library(mixedFlowR)
d <- mixed_data("splitplot")
head(d)

## ----capabilities-------------------------------------------------------------
mixed_capabilities()

## ----audit--------------------------------------------------------------------
audit <- mixed_design_audit(
  d,
  response = "yield",
  block = "block",
  whole = "variety",
  sub = "nitrogen"
)
audit

## ----hierarchy----------------------------------------------------------------
mixed_hierarchy(d, block = "block", whole = "variety", sub = "nitrogen")

## ----balance------------------------------------------------------------------
mixed_balance(d, c("block", "variety", "nitrogen"))

## ----whole-plots--------------------------------------------------------------
d$whole_plot <- interaction(d$block, d$variety, drop = TRUE)

mixed_pseudorep(
  d,
  response = "yield",
  cluster = "whole_plot",
  treatment = "variety"
)

## ----raw-plot, fig.height=5.2-------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(
    d,
    ggplot2::aes(nitrogen, yield, colour = variety)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
    ggplot2::facet_wrap(~ block) +
    ggplot2::labs(
      x = "Nitrogen dose",
      y = "Yield",
      colour = "Variety",
      title = "Observed split-plot response by block"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

## ----descriptive--------------------------------------------------------------
raw_means <- aggregate(yield ~ variety + nitrogen, d, mean)
raw_means

## ----legacy-data--------------------------------------------------------------
d$nitrogen_f <- factor(d$nitrogen)

## ----legacy-fit---------------------------------------------------------------
legacy <- fit_legacy_splitplot(
  d,
  response = "yield",
  block = "block",
  whole = "variety",
  sub = "nitrogen_f"
)

## ----legacy-table-------------------------------------------------------------
splitplot_anova(legacy)

## ----legacy-ems---------------------------------------------------------------
splitplot_ems(d, "block", "variety", "nitrogen_f")

## ----legacy-mixed-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  compare_legacy_mixed(
    d, "yield", "block", "variety", "nitrogen_f"
  )
}

## ----linear-fit---------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_linear <- mixed_fit(
    yield ~ variety * nitrogen +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

## ----linear-diagnose----------------------------------------------------------
if (exists("fit_linear")) {
  diag_linear <- mixed_diagnose(fit_linear)
  diag_linear
  mixed_plot(fit_linear, type = "residuals")
}

## ----quadratic-fit------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_quad <- mixed_fit(
    yield ~ variety * poly(nitrogen, 2) +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

## ----quadratic-diagnose-------------------------------------------------------
if (exists("fit_quad")) {
  diag_quad <- mixed_diagnose(fit_quad)
  diag_quad
  mixed_plot(fit_quad, type = "residuals")
}

## ----ml-fits------------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  linear_ml <- mixed_fit(
    yield ~ variety * nitrogen +
      (1 | block) + (1 | block:variety),
    data = d,
    REML = FALSE
  )

  quad_ml <- mixed_fit(
    yield ~ variety * poly(nitrogen, 2) +
      (1 | block) + (1 | block:variety),
    data = d,
    REML = FALSE
  )
}

## ----mean-comparison----------------------------------------------------------
if (exists("linear_ml") && exists("quad_ml")) {
  mixed_compare(
    linear = linear_ml,
    quadratic = quad_ml,
    method = "information"
  )
}

## ----kr-anova-----------------------------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("lmerTest", quietly = TRUE) &&
  requireNamespace("pbkrtest", quietly = TRUE)
) {
  mixed_anova(
    fit_quad,
    ddf = "Kenward-Roger"
  )
}

## ----variety-emm--------------------------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  variety_emm <- mixed_emmeans(
    fit_quad,
    specs = ~ variety
  )
  variety_emm
}

## ----variety-contrasts--------------------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_contrasts(
    fit_quad,
    specs = ~ variety,
    method = "pairwise",
    adjust = "tukey"
  )
}

## ----quantitative-helper------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  qfit <- mixed_quantitative(
    d,
    response = "yield",
    quantitative = "nitrogen",
    qualitative = "variety",
    group = "block",
    degree = 2,
    basis = "polynomial"
  )
}

## ----slopes-------------------------------------------------------------------
if (
  exists("fit_linear") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  slopes <- mixed_trend(
    fit_linear,
    specs = ~ variety,
    var = "nitrogen"
  )
  slopes
}

## ----curve-data---------------------------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  curve_data <- mixed_curve(
    fit_quad,
    variable = "nitrogen",
    values = seq(0, 150, by = 5),
    by = "variety"
  )

  head(curve_data)
}

## ----curve-plot, fig.height=5.2-----------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_plot(
    fit_quad,
    type = "curve",
    x = "nitrogen",
    by = "variety",
    values = seq(0, 150, by = 2.5),
    show_data = TRUE
  )
}

## ----optimum------------------------------------------------------------------
if (
  exists("fit_quad") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  opt <- mixed_optimum(
    fit_quad,
    variable = "nitrogen",
    range = c(0, 150),
    by = "variety",
    objective = "max",
    n = 501
  )

  opt
}

## ----robust-------------------------------------------------------------------
if (
  exists("fit_linear") &&
  requireNamespace("robustlmm", quietly = TRUE)
) {
  robust_fit <- mixed_robust(
    yield ~ variety * nitrogen +
      (1 | block) +
      (1 | block:variety),
    data = d
  )

  mixed_compare_robust(
    fit_linear,
    robust_fit
  )
}

## ----contamination------------------------------------------------------------
if (exists("fit_linear")) {
  mixed_contamination(
    fit_linear,
    cluster = "whole_plot",
    threshold = 2.5
  )
}

## ----boot-demo----------------------------------------------------------------
if (exists("fit_linear")) {
  boot_demo <- mixed_boot_fixed(
    fit_linear,
    B = 49,
    seed = 260818
  )
}

## ----boot-production, eval=FALSE----------------------------------------------
# boot_full <- mixed_boot_fixed(
#   fit_linear,
#   B = 4999,
#   seed = 260818
# )

## ----boot-var-demo------------------------------------------------------------
if (exists("fit_linear")) {
  boot_var_demo <- mixed_boot_variance(
    fit_linear,
    B = 49,
    seed = 260818
  )
}

## ----gamlss-sensitivity-------------------------------------------------------
if (requireNamespace("gamlss", quietly = TRUE)) {
  g_loc <- mixed_gamlss(
    yield ~ variety * nitrogen,
    d,
    family = "NO",
    random = "block"
  )

  g_scale <- mixed_gamlss(
    yield ~ variety * nitrogen,
    d,
    family = "NO",
    sigma.formula = ~ variety,
    random = "block"
  )

  mixed_gamlss_select(
    constant_scale = g_loc,
    variety_scale = g_scale
  )
}

## ----bayes-template, eval=FALSE-----------------------------------------------
# priors <- c(
#   brms::prior(normal(0, 20), class = "Intercept"),
#   brms::prior(normal(0, 5), class = "b"),
#   brms::prior(exponential(1), class = "sd"),
#   brms::prior(exponential(1), class = "sigma")
# )
# 
# bayes_fit <- mixed_bayes(
#   yield ~ variety * nitrogen +
#     (1 | block) +
#     (1 | block:variety),
#   data = d,
#   prior = priors,
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )
# 
# mixed_pp_check(bayes_fit)

## ----fixed-table--------------------------------------------------------------
if (exists("fit_linear")) {
  mixed_table(
    fit_linear,
    component = "fixed",
    format = "markdown",
    digits = 3,
    caption = "Fixed-effect estimates for the design-correct LMM"
  )
}

## ----random-table-------------------------------------------------------------
if (exists("fit_linear")) {
  mixed_table(
    fit_linear,
    component = "random",
    format = "markdown",
    digits = 3,
    caption = "Random-effect variance components"
  )
}

## ----diagnostics-table--------------------------------------------------------
if (exists("diag_linear")) {
  mixed_table(
    diag_linear,
    component = "diagnostics",
    format = "markdown",
    caption = "Diagnostic status"
  )
}

## ----export, eval=FALSE-------------------------------------------------------
# mixed_plot(
#   fit_quad,
#   type = "curve",
#   x = "nitrogen",
#   by = "variety",
#   values = seq(0, 150, by = 2.5),
#   show_data = TRUE,
#   file = "yield_nitrogen_curve.tiff",
#   width = 7,
#   height = 5,
#   dpi = 600
# )

## ----report-------------------------------------------------------------------
if (exists("fit_linear")) {
  report_file <- tempfile(fileext = ".md")
  mixed_report(
    fit_linear,
    report_file,
    title = "Split-plot variety × nitrogen analysis"
  )

  readLines(report_file, n = 12)
}

## ----reproducibility----------------------------------------------------------
utils::sessionInfo()

