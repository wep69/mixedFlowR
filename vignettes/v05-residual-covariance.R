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

## ----design-summary-----------------------------------------------------------
with(z, table(treatment, time))
length(unique(z$subject))

## ----trajectories-------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(z, ggplot2::aes(time, height, group = subject, colour = treatment)) +
    ggplot2::geom_line(alpha = 0.28) +
    ggplot2::geom_point(alpha = 0.45, size = 1) +
    ggplot2::stat_summary(
      ggplot2::aes(group = treatment),
      fun = mean,
      geom = "line",
      linewidth = 1.1
    ) +
    ggplot2::labs(x = "Time", y = "Height", colour = "Treatment") +
    ggplot2::theme_minimal(base_size = 11)
}

## ----random-intercept---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  ri <- mixed_random_intercepts(
    z,
    response = "height",
    fixed    = "treatment * time",
    group    = "subject"
  )
}

## ----covariance-specs---------------------------------------------------------
ar1_spec <- mixed_covariance("ar1", time = "time", group = "subject")
car1_spec <- mixed_covariance("car1", time = "time", group = "subject")
toep_spec <- mixed_covariance("toep", time = "time", group = "subject")

ar1_spec
car1_spec
toep_spec

## ----spatial-spec-------------------------------------------------------------
mixed_covariance(
  type    = "mat",
  group   = "field",
  spatial = c("row", "col")
)

## ----ar1----------------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  m_ar1 <- mixed_ar1(
    data     = z,
    response = "height",
    fixed    = "treatment * time",
    time     = "time",
    group    = "subject",
    engine   = "nlme"
  )

  m_ar1
}

## ----irregular-data-----------------------------------------------------------
z_irregular <- z[!(z$subject == "S01" & z$time == 2), ]

## ----car1---------------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  m_car1 <- mixed_car1(
    data     = z_irregular,
    response = "height",
    fixed    = "treatment * time",
    time     = "time",
    group    = "subject"
  )

  m_car1
}

## ----heterogeneity------------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  m_het <- mixed_heterogeneity(
    height ~ treatment * time,
    data           = z,
    random         = ~ 1 | subject,
    variance_group = "treatment"
  )

  m_het
}

## ----heterogeneity-ar1--------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  m_het_ar1 <- mixed_heterogeneity(
    height ~ treatment * time,
    data           = z,
    random         = ~ 1 | subject,
    variance_group = "treatment",
    correlation    = nlme::corAR1(form = ~ time | subject)
  )
}

## ----toeplitz-----------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_toep <- mixed_toeplitz(
    data     = z,
    response = "height",
    fixed    = "treatment * time",
    time     = "time",
    group    = "subject"
  )

  m_toep
}

## ----ar1-structured-lme4------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  m_ar1_lme4 <- mixed_ar1(
    z, "height", "treatment * time", "time", "subject",
    engine = "lme4"
  )
}

## ----ar1-structured-tmb-------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_ar1_tmb <- mixed_ar1(
    z, "height", "treatment * time", "time", "subject",
    engine = "glmmTMB"
  )
}

## ----covariance-compare-------------------------------------------------------
if (exists("m_ar1") && exists("m_het_ar1")) {
  mixed_covariance_compare(
    AR1 = m_ar1,
    Heterogeneous_AR1 = m_het_ar1
  )
}

## ----random-slope-covariance--------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rs <- mixed_random_slopes(
    z,
    response   = "height",
    fixed      = "treatment * time",
    slope      = "time",
    group      = "subject",
    correlated = TRUE
  )

  mixed_covariance_plot(
    rs,
    group = "subject",
    correlation = TRUE
  )
}

## ----temporal-grid------------------------------------------------------------
grid_time <- mixed_temporal_grid(
  z,
  time = "time",
  by   = "treatment",
  n    = 60
)
head(grid_time)

## ----ou-----------------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_ou <- mixed_spatiotemporal(
    data      = z,
    response  = "height",
    fixed     = "treatment * time",
    time      = "time",
    group     = "subject",
    structure = "ou"
  )
}

## ----diagnose-ar1-------------------------------------------------------------
if (exists("m_ar1")) {
  mixed_diagnose(m_ar1)
}

## ----diagnose-toeplitz--------------------------------------------------------
if (exists("m_toep")) {
  mixed_diagnose(m_toep)
}

## ----time-specific-variance---------------------------------------------------
z$time_f <- factor(z$time)

if (requireNamespace("nlme", quietly = TRUE)) {
  m_timevar <- mixed_heterogeneity(
    height ~ treatment * time,
    data           = z,
    random         = ~ 1 | subject,
    variance_group = "time_f"
  )
}

