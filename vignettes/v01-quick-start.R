## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  warning = FALSE,
  message = FALSE,
  error = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  dpi = 150
)
set.seed(260818)

## ----capabilities-------------------------------------------------------------
library(mixedFlowR)

caps <- mixed_capabilities()
caps

## ----block-map----------------------------------------------------------------
mixed_blocks()

## ----block-search-------------------------------------------------------------
mixed_blocks("Bootstrap")
mixed_blocks("Spatial|field")
mixed_blocks("GAMLSS|Bayesian")

## ----data---------------------------------------------------------------------
d <- mixed_data("splitplot")
str(d)
head(d)

## ----descriptive-table--------------------------------------------------------
aggregate(yield ~ variety + nitrogen, data = d, FUN = mean)

## ----raw-figure---------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(d, ggplot2::aes(nitrogen, yield, group = variety, colour = variety)) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
    ggplot2::labs(
      x = "Nitrogen dose",
      y = "Yield",
      colour = "Variety",
      title = "Observed yield across nitrogen doses"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

## ----design-audit-------------------------------------------------------------
audit <- mixed_design_audit(
  data     = d,
  response = "yield",
  block    = "block",
  whole    = "variety",
  sub      = "nitrogen"
)

audit

## ----hierarchy----------------------------------------------------------------
mixed_hierarchy(
  d,
  block = "block",
  whole = "variety",
  sub   = "nitrogen"
)

## ----balance------------------------------------------------------------------
mixed_balance(d, c("block", "variety", "nitrogen"))

## ----pseudorep----------------------------------------------------------------
d$whole_plot <- interaction(d$block, d$variety, drop = TRUE)

mixed_pseudorep(
  d,
  response  = "yield",
  cluster   = "whole_plot",
  treatment = "variety"
)

## ----legacy-prepare-----------------------------------------------------------
d$nitrogen_f <- factor(d$nitrogen)

## ----legacy-fit---------------------------------------------------------------
legacy <- fit_legacy_splitplot(
  data     = d,
  response = "yield",
  block    = "block",
  whole    = "variety",
  sub      = "nitrogen_f"
)

splitplot_anova(legacy)

## ----ems----------------------------------------------------------------------
splitplot_ems(
  data  = d,
  block = "block",
  whole = "variety",
  sub   = "nitrogen_f"
)

## ----legacy-vs-mixed----------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  compare_legacy_mixed(
    data     = d,
    response = "yield",
    block    = "block",
    whole    = "variety",
    sub      = "nitrogen_f"
  )
}

## ----lmm-fit------------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- mixed_fit(
    yield ~ variety * nitrogen +
      (1 | block) +
      (1 | block:variety),
    data = d,
    engine = "lme4"
  )

  fit
}

## ----lmm-diagnostics----------------------------------------------------------
if (exists("fit")) {
  diag <- mixed_diagnose(fit)
  diag
}

## ----lmm-residual-plot, fig.height=4.8----------------------------------------
if (exists("fit")) {
  mixed_plot(fit, type = "residuals")
}

## ----satterthwaite------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("lmerTest", quietly = TRUE)
) {
  mixed_anova(fit, ddf = "Satterthwaite")
}

## ----kenward-roger------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("lmerTest", quietly = TRUE) &&
  requireNamespace("pbkrtest", quietly = TRUE)
) {
  mixed_anova(fit, ddf = "Kenward-Roger")
}

## ----emmeans------------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  emm_variety <- mixed_emmeans(
    fit,
    specs = ~ variety
  )

  emm_variety
}

## ----contrasts----------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_contrasts(
    fit,
    specs  = ~ variety,
    method = "pairwise",
    adjust = "tukey"
  )
}

## ----trend--------------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_trend(
    fit,
    specs = ~ variety,
    var   = "nitrogen"
  )
}

## ----quantitative-fit---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  qfit <- mixed_quantitative(
    data         = d,
    response     = "yield",
    quantitative = "nitrogen",
    qualitative  = "variety",
    group        = "block",
    degree       = 2,
    basis        = "polynomial"
  )
}

## ----response-curve, fig.height=4.8-------------------------------------------
if (
  exists("qfit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_plot(
    qfit,
    type      = "curve",
    x         = "nitrogen",
    by        = "variety",
    values    = seq(min(d$nitrogen), max(d$nitrogen), length.out = 61),
    show_data = TRUE
  )
}

## ----fixed-table--------------------------------------------------------------
if (exists("fit")) {
  mixed_table(
    fit,
    component = "fixed",
    format    = "markdown",
    digits    = 3,
    caption   = "Fixed-effect estimates for the split-plot LMM"
  )
}

## ----random-table-------------------------------------------------------------
if (exists("fit")) {
  mixed_table(
    fit,
    component = "random",
    format    = "markdown",
    digits    = 3,
    caption   = "Random-effect variance components"
  )
}

## ----report, eval=FALSE-------------------------------------------------------
# # This writes a new Markdown file in the working directory.
# mixed_report(
#   fit,
#   file  = "quick_start_analysis.md",
#   title = "mixedFlowR quick-start analysis"
# )

## ----exercise-1---------------------------------------------------------------
d_missing <- d[-1, ]
mixed_balance(d_missing, c("block", "variety", "nitrogen"))

## ----exercise-2---------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_factor <- mixed_fit(
    yield ~ variety * nitrogen_f +
      (1 | block) +
      (1 | block:variety),
    data = d
  )

  mixed_diagnose(fit_factor)
}

## ----exercise-3---------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_wrong_scale <- mixed_fit(
    yield ~ variety * nitrogen + (1 | block),
    data = d
  )
}

