## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----data-model---------------------------------------------------------------
library(mixedFlowR)
z <- mixed_data("longitudinal")

if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- mixed_fit(
    height ~ treatment * time + (time | subject),
    data = z
  )
}

## ----fixed-dataframe----------------------------------------------------------
if (exists("fit")) {
  tab_fixed <- mixed_table(
    fit,
    component = "fixed",
    format = "data.frame"
  )

  tab_fixed
}

## ----fixed-markdown-----------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("knitr", quietly = TRUE)
) {
  mixed_table(
    fit,
    component = "fixed",
    format = "markdown",
    digits = 3,
    caption = "Fixed-effect estimates"
  )
}

## ----random-table-------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("knitr", quietly = TRUE)
) {
  mixed_table(
    fit,
    component = "random",
    format = "markdown",
    digits = 3,
    caption = "Random-effect variance components"
  )
}

## ----anova-table--------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("knitr", quietly = TRUE)
) {
  mixed_table(
    fit,
    component = "anova",
    format = "markdown",
    digits = 3,
    caption = "Model ANOVA table"
  )
}

## ----emm-table----------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("emmeans", quietly = TRUE) &&
  requireNamespace("knitr", quietly = TRUE)
) {
  mixed_table(
    fit,
    component = "emmeans",
    format = "markdown",
    specs = ~ treatment,
    caption = "Estimated marginal means by treatment"
  )
}

## ----flextable----------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("flextable", quietly = TRUE)
) {
  ft <- mixed_table(
    fit,
    component = "fixed",
    format = "flextable",
    digits = 3,
    caption = "Fixed-effect estimates"
  )

  ft
}

## ----raw-plot-----------------------------------------------------------------
if (exists("fit")) {
  mixed_plot(
    fit,
    type = "raw",
    x = "time",
    by = "treatment"
  )
}

## ----residual-plot------------------------------------------------------------
if (exists("fit")) {
  mixed_plot(fit, type = "residuals")
}

## ----fixed-plot---------------------------------------------------------------
if (exists("fit")) {
  mixed_plot(fit, type = "fixed")
}

## ----random-plot--------------------------------------------------------------
if (exists("fit")) {
  mixed_plot(fit, type = "random")
}

## ----curve-plot---------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_plot(
    fit,
    type = "curve",
    x = "time",
    by = "treatment",
    values = seq(min(z$time), max(z$time), by = 0.1),
    show_data = TRUE
  )
}

## ----covariance-plot----------------------------------------------------------
if (exists("fit")) {
  mixed_plot(
    fit,
    type = "covariance",
    group = "subject",
    correlation = TRUE
  )
}

## ----bootstrap-object---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_ri <- mixed_fit(height ~ treatment * time + (1 | subject), z)
  boot_demo <- mixed_boot_fixed(fit_ri, B = 49, seed = 260818)
}

## ----bootstrap-plot-----------------------------------------------------------
if (exists("boot_demo")) {
  mixed_plot(boot_demo, type = "bootstrap")
}

## ----spatial-object-----------------------------------------------------------
s <- mixed_data("spatial")

if (requireNamespace("SpATS", quietly = TRUE)) {
  sp <- mixed_spatial_field(
    s,
    "yield",
    "genotype",
    "col",
    "row",
    random = ~ block
  )
}

## ----spatial-plot-------------------------------------------------------------
if (exists("sp")) {
  mixed_plot(sp, type = "spatial")
}

## ----export-tiff, eval=FALSE--------------------------------------------------
# mixed_plot(
#   fit,
#   type = "residuals",
#   file = "residual_diagnostics.tiff",
#   width = 7,
#   height = 5,
#   dpi = 600
# )

## ----export-svg, eval=FALSE---------------------------------------------------
# mixed_plot(
#   fit,
#   type = "fixed",
#   file = "fixed_effects.svg",
#   width = 7,
#   height = 5
# )

## ----export-pdf, eval=FALSE---------------------------------------------------
# mixed_plot(
#   fit,
#   type = "curve",
#   x = "time",
#   by = "treatment",
#   values = seq(0, 5, by = 0.1),
#   file = "treatment_curves.pdf",
#   width = 7,
#   height = 5
# )

## ----report-temp--------------------------------------------------------------
if (exists("fit")) {
  tmp_report <- tempfile(fileext = ".md")
  mixed_report(
    fit,
    file = tmp_report,
    title = "Longitudinal mixed-model analysis"
  )

  readLines(tmp_report, n = 12)
}

