## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----growth-data--------------------------------------------------------------
library(mixedFlowR)
g <- mixed_data("growth")
head(g)

## ----growth-plot--------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(g, ggplot2::aes(time, biomass, group = plant, colour = treatment)) +
    ggplot2::geom_line(alpha = 0.25) +
    ggplot2::geom_point(alpha = 0.45, size = 1) +
    ggplot2::stat_summary(
      ggplot2::aes(group = treatment),
      fun = mean,
      geom = "line",
      linewidth = 1.1
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(x = "Time", y = "Biomass", colour = "Treatment")
}

## ----logistic-----------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  logis_fit <- mixed_growth(
    data     = g,
    response = "biomass",
    time     = "time",
    group    = "plant",
    model    = "logistic"
  )

  logis_fit
}

## ----gompertz-----------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  gomp_fit <- mixed_growth(
    data     = g,
    response = "biomass",
    time     = "time",
    group    = "plant",
    model    = "gompertz"
  )

  gomp_fit
}

## ----growth-compare-----------------------------------------------------------
if (exists("logis_fit") && exists("gomp_fit")) {
  mixed_compare(
    Logistic = logis_fit,
    Gompertz = gomp_fit,
    method = "information"
  )
}

## ----growth-diagnose----------------------------------------------------------
if (exists("logis_fit")) {
  mixed_diagnose(logis_fit)
  mixed_plot(logis_fit, type = "residuals")
}

## ----initial------------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  st <- stats::getInitial(
    biomass ~ SSlogis(time, Asym, xmid, scal),
    data = g
  )
  st
}

## ----generic-nlme-------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  generic_logis <- mixed_nonlinear(
    biomass ~ SSlogis(time, Asym, xmid, scal),
    data   = g,
    engine = "nlme",
    fixed  = Asym + xmid + scal ~ 1,
    random = Asym ~ 1,
    groups = ~ plant,
    start  = st
  )
}

## ----generic-lme4-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  st_lme4 <- stats::getInitial(
    biomass ~ SSlogis(time, Asym, xmid, scal),
    data = g
  )

  lme4_nl <- mixed_nonlinear(
    biomass ~ SSlogis(time, Asym, xmid, scal) ~ (Asym | plant),
    data   = g,
    engine = "lme4",
    start  = st_lme4
  )
}

## ----dose-data----------------------------------------------------------------
dd <- mixed_data("dose_response")
head(dd)

## ----dose-summary-------------------------------------------------------------
aggregate(response ~ dose, dd, mean)

## ----dose-plot----------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(dd, ggplot2::aes(dose, response, group = unit)) +
    ggplot2::geom_line(alpha = 0.12) +
    ggplot2::geom_point(alpha = 0.25) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 1.1, colour = "black") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(x = "Dose", y = "Response")
}

## ----dose-fit-----------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  dose_fit <- mixed_dose_response(
    data     = dd,
    response = "response",
    dose     = "dose",
    group    = "unit"
  )

  dose_fit
}

## ----dose-start---------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  dose_start <- stats::getInitial(
    response ~ SSfpl(dose, A, B, xmid, scal),
    data = dd
  )

  dose_fit_start <- mixed_dose_response(
    dd,
    response = "response",
    dose = "dose",
    group = "unit",
    start = dose_start
  )
}

## ----random-regression-linear-------------------------------------------------
z <- mixed_data("longitudinal")

if (requireNamespace("lme4", quietly = TRUE)) {
  rr1 <- mixed_random_curve(
    response = "height",
    time     = "time",
    data     = z,
    group    = "subject",
    degree   = 1,
    fixed    = "treatment"
  )
}

## ----random-regression-quadratic----------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rr2 <- mixed_random_curve(
    response = "height",
    time     = "time",
    data     = z,
    group    = "subject",
    degree   = 2,
    fixed    = "treatment"
  )
}

## ----rr-compare---------------------------------------------------------------
if (exists("rr1") && exists("rr2")) {
  mixed_compare(
    linear    = rr1,
    quadratic = rr2,
    method = "information"
  )
}

## ----bayes-nonlinear, eval=FALSE----------------------------------------------
# bf_growth <- brms::bf(
#   biomass ~ Asym / (1 + exp((xmid - time) / scal)),
#   Asym ~ 1 + (1 | plant),
#   xmid ~ 1,
#   scal ~ 1,
#   nl = TRUE
# )
# 
# bayes_growth <- mixed_nonlinear(
#   bf_growth,
#   data = g,
#   engine = "brms",
#   prior = nonlinear_priors,  # define priors on biologically interpretable parameters first
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )

