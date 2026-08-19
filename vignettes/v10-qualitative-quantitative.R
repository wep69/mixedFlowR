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

## ----raw-summary--------------------------------------------------------------
aggregate(yield ~ variety + nitrogen, d, mean)

## ----polynomial-fit-----------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_poly <- mixed_quantitative(
    data         = d,
    response     = "yield",
    quantitative = "nitrogen",
    qualitative  = "variety",
    group        = "block",
    degree       = 2,
    basis        = "polynomial"
  )
}

## ----emmeans------------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  emm <- mixed_emmeans(
    fit_poly,
    specs = ~ variety
  )

  emm
}

## ----pairwise-----------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  pairs <- mixed_contrasts(
    fit_poly,
    specs  = ~ variety,
    method = "pairwise",
    adjust = "tukey"
  )

  pairs
}

## ----cld----------------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE) &&
  requireNamespace("multcomp", quietly = TRUE) &&
  requireNamespace("multcompView", quietly = TRUE)
) {
  cld <- mixed_cld(
    fit_poly,
    specs  = ~ variety,
    adjust = "tukey"
  )

  cld
}

## ----simple-trend-fit---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_linear <- mixed_fit(
    yield ~ variety * nitrogen +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

## ----trend--------------------------------------------------------------------
if (
  exists("fit_linear") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  tr <- mixed_trend(
    fit_linear,
    specs = ~ variety,
    var   = "nitrogen"
  )

  tr
}

## ----curve--------------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  curve_tab <- mixed_curve(
    fit_poly,
    variable = "nitrogen",
    values   = seq(0, 150, by = 10),
    by       = "variety"
  )

  head(curve_tab)
}

## ----curve-plot---------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_plot(
    fit_poly,
    type      = "curve",
    x         = "nitrogen",
    by        = "variety",
    values    = seq(0, 150, by = 2.5),
    show_data = TRUE
  )
}

## ----optimum------------------------------------------------------------------
if (
  exists("fit_poly") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  opt <- mixed_optimum(
    fit_poly,
    variable  = "nitrogen",
    range     = c(0, 150),
    by        = "variety",
    objective = "max",
    n         = 501
  )

  opt
}

## ----linear-basis-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  q_linear <- mixed_quantitative(
    d,
    response     = "yield",
    quantitative = "nitrogen",
    qualitative  = "variety",
    group        = "block",
    degree       = 1,
    basis        = "linear"
  )
}

## ----spline-fit---------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  q_spline <- mixed_quantitative(
    d,
    response     = "yield",
    quantitative = "nitrogen",
    qualitative  = "variety",
    group        = "block",
    basis        = "spline",
    df           = 4
  )
}

## ----mean-compare-------------------------------------------------------------
if (exists("q_linear") && exists("fit_poly")) {
  mixed_compare(
    linear    = q_linear,
    quadratic = fit_poly,
    method = "information"
  )
}

## ----dose-factor--------------------------------------------------------------
d$nitrogen_f <- factor(d$nitrogen)

## ----factor-model-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_factor <- mixed_fit(
    yield ~ variety * nitrogen_f +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

## ----fixed-table--------------------------------------------------------------
if (exists("fit_poly")) {
  mixed_table(
    fit_poly,
    component = "fixed",
    format = "markdown",
    caption = "Fixed-effect coefficients for the quantitative-treatment model"
  )
}

