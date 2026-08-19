## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----data---------------------------------------------------------------------
library(mixedFlowR)
z <- mixed_data("longitudinal")

## ----lmm----------------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- mixed_fit(
    height ~ treatment * time + (time | subject),
    data = z
  )
}

## ----diagnose-----------------------------------------------------------------
if (exists("fit")) {
  diag <- mixed_diagnose(fit)
  diag
}

## ----residual-plot------------------------------------------------------------
if (exists("fit")) {
  mixed_plot(fit, type = "residuals")
}

## ----demanding-fit------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  z_sparse <- subset(z, time %in% c(0, 1, 5))
  demanding <- mixed_random_slopes(
    z_sparse,
    response = "height",
    fixed = "treatment * time",
    slope = "time",
    group = "subject",
    correlated = TRUE
  )
  mixed_diagnose(demanding)
}

## ----count-model--------------------------------------------------------------
cnt <- mixed_data("counts")

if (requireNamespace("glmmTMB", quietly = TRUE)) {
  nb <- mixed_count(
    count ~ treatment * time + (1 | plot),
    data = cnt,
    family = "nbinom2"
  )
}

## ----count-dharma-------------------------------------------------------------
if (
  exists("nb") &&
  requireNamespace("DHARMa", quietly = TRUE)
) {
  nb_diag <- mixed_diagnose(nb, simulate = TRUE, nsim = 500)
  nb_diag$simulation
}

## ----influence----------------------------------------------------------------
if (
  exists("fit") &&
  requireNamespace("influence.ME", quietly = TRUE)
) {
  infl <- mixed_influence(
    fit,
    group = "subject"
  )

  infl
}

## ----ml-models----------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  m0_ml <- mixed_fit(
    height ~ time + (1 | subject),
    data = z,
    REML = FALSE
  )

  m1_ml <- mixed_fit(
    height ~ treatment + time + (1 | subject),
    data = z,
    REML = FALSE
  )
}

## ----information--------------------------------------------------------------
if (exists("m0_ml") && exists("m1_ml")) {
  mixed_compare(
    no_treatment = m0_ml,
    treatment    = m1_ml,
    method       = "information"
  )
}

## ----lrt----------------------------------------------------------------------
if (exists("m0_ml") && exists("m1_ml")) {
  mixed_compare(
    m0_ml,
    m1_ml,
    method = "LRT"
  )
}

## ----invalid-reml-lrt, eval=FALSE---------------------------------------------
# m0_reml <- mixed_fit(height ~ time + (1 | subject), z, REML = TRUE)
# m1_reml <- mixed_fit(height ~ treatment + time + (1 | subject), z, REML = TRUE)
# mixed_compare(m0_reml, m1_reml, method = "LRT")

## ----kr-models----------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  kr_small <- mixed_fit(
    height ~ time + (1 | subject),
    data = z
  )

  kr_large <- mixed_fit(
    height ~ treatment + time + (1 | subject),
    data = z
  )
}

## ----kr-----------------------------------------------------------------------
if (
  exists("kr_small") && exists("kr_large") &&
  requireNamespace("pbkrtest", quietly = TRUE)
) {
  mixed_compare(
    kr_small,
    kr_large,
    method = "Kenward-Roger"
  )
}

## ----pb-demo------------------------------------------------------------------
if (
  exists("kr_small") && exists("kr_large") &&
  requireNamespace("pbkrtest", quietly = TRUE)
) {
  pb_demo <- mixed_compare(
    kr_small,
    kr_large,
    method = "parametric-bootstrap",
    nsim = 99,
    seed = 260818
  )

  pb_demo
}

## ----pb-production, eval=FALSE------------------------------------------------
# pb_full <- mixed_compare(
#   kr_small,
#   kr_large,
#   method = "parametric-bootstrap",
#   nsim = 4999,
#   seed = 260818
# )

## ----covariance-models--------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  ar1 <- mixed_ar1(z, "height", "treatment*time", "time", "subject")
  car1 <- mixed_car1(z, "height", "treatment*time", "time", "subject")
}

## ----covariance-compare-------------------------------------------------------
if (exists("ar1") && exists("car1")) {
  mixed_covariance_compare(
    AR1 = ar1,
    CAR1 = car1
  )
}

## ----diagnostics-table--------------------------------------------------------
if (exists("diag")) {
  mixed_table(
    diag,
    component = "diagnostics",
    format = "markdown",
    caption = "Mixed-model diagnostic summary"
  )
}

