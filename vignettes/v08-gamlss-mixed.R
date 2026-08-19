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
head(z)

## ----variability--------------------------------------------------------------
aggregate(height ~ treatment, z, function(x) c(mean = mean(x), sd = sd(x)))

## ----gamlss-baseline----------------------------------------------------------
if (requireNamespace("gamlss", quietly = TRUE)) {
  g1 <- mixed_gamlss(
    height ~ treatment * time,
    data   = z,
    family = "NO",
    random = "subject"
  )

  g1
}

## ----gamlss-scale-------------------------------------------------------------
if (requireNamespace("gamlss", quietly = TRUE)) {
  g2 <- mixed_gamlss(
    height ~ treatment * time,
    data          = z,
    family        = "NO",
    sigma.formula = ~ treatment,
    random        = "subject"
  )

  g2
}

## ----parameters---------------------------------------------------------------
if (exists("g2")) {
  pars <- mixed_gamlss_parameters(
    g2,
    parameters = c("mu", "sigma")
  )

  head(pars)
}

## ----compare------------------------------------------------------------------
if (exists("g1") && exists("g2")) {
  mixed_gamlss_select(
    constant_scale = g1,
    treatment_scale = g2,
    k = 2
  )
}

## ----bic-like-----------------------------------------------------------------
if (exists("g1") && exists("g2")) {
  mixed_gamlss_select(
    constant_scale = g1,
    treatment_scale = g2,
    k = log(nrow(z))
  )
}

## ----quantiles----------------------------------------------------------------
if (
  exists("g2") &&
  requireNamespace("gamlss.dist", quietly = TRUE)
) {
  qfit <- mixed_gamlss_quantiles(
    g2,
    probs = c(0.10, 0.50, 0.90)
  )

  head(qfit)
}

## ----diagnostics--------------------------------------------------------------
if (exists("g2")) {
  gd <- mixed_gamlss_diagnose(g2)
  gd$summary
  gd$plot
}

## ----re-random----------------------------------------------------------------
if (
  requireNamespace("gamlss", quietly = TRUE) &&
  requireNamespace("nlme", quietly = TRUE)
) {
  g_re <- mixed_gamlss(
    height ~ treatment * time,
    data      = z,
    family    = "NO",
    re_random = ~ 1 | subject
  )
}

## ----gamlss-correlation-------------------------------------------------------
if (
  requireNamespace("gamlss", quietly = TRUE) &&
  requireNamespace("nlme", quietly = TRUE)
) {
  g_cor <- mixed_gamlss(
    height ~ treatment * time,
    data        = z,
    family      = "NO",
    re_random   = ~ 1 | subject,
    correlation = nlme::corAR1(form = ~ time | subject)
  )
}

## ----shape-example, eval=FALSE------------------------------------------------
# g_shape <- mixed_gamlss(
#   y ~ treatment + time,
#   data = dat,
#   family = chosen_four_parameter_family,  # choose a supported family from the scientific question
#   sigma.formula = ~ treatment,
#   nu.formula = ~ treatment,
#   tau.formula = ~ 1,
#   random = "subject"
# )

## ----parameter-table----------------------------------------------------------
if (exists("g2")) {
  head(mixed_gamlss_parameters(g2, c("mu", "sigma")), 12)
}

