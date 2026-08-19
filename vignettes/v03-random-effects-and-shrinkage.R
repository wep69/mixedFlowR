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

## ----data---------------------------------------------------------------------
library(mixedFlowR)
z <- mixed_data("longitudinal")
head(z)

## ----structure----------------------------------------------------------------
with(z, table(treatment, time))
length(unique(z$subject))

## ----raw-trajectories---------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(
    z,
    ggplot2::aes(time, height, group = subject, colour = treatment)
  ) +
    ggplot2::geom_line(alpha = 0.35) +
    ggplot2::geom_point(alpha = 0.45, size = 1.2) +
    ggplot2::labs(
      x = "Time",
      y = "Height",
      colour = "Treatment",
      title = "Repeated subject trajectories"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

## ----random-intercept---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  ri <- mixed_random_intercepts(
    data     = z,
    response = "height",
    fixed    = "treatment * time",
    group    = "subject"
  )

  ri
}

## ----random-intercept-core----------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  ri_core <- mixed_fit(
    height ~ treatment * time + (1 | subject),
    data = z
  )
}

## ----random-slope-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rs <- mixed_random_slopes(
    data       = z,
    response   = "height",
    fixed      = "treatment * time",
    slope      = "time",
    group      = "subject",
    correlated = TRUE
  )

  rs
}

## ----random-slope-diagonal----------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rs_diag <- mixed_random_slopes(
    data       = z,
    response   = "height",
    fixed      = "treatment * time",
    slope      = "time",
    group      = "subject",
    correlated = FALSE
  )
}

## ----covariance-plot----------------------------------------------------------
if (exists("rs")) {
  mixed_covariance_plot(
    rs,
    group = "subject",
    correlation = TRUE
  )
}

## ----blups--------------------------------------------------------------------
if (exists("rs")) {
  blup <- mixed_blup(rs, group = "subject")
  head(blup)
}

## ----random-plot--------------------------------------------------------------
if (exists("rs")) {
  mixed_plot(rs, type = "random")
}

## ----raw-subject-means--------------------------------------------------------
raw_subject <- aggregate(height ~ subject, data = z, FUN = mean)
head(raw_subject)

## ----random-effects-table-----------------------------------------------------
if (exists("ri")) {
  re_subject <- mixed_blup(ri, group = "subject")
  head(re_subject)
}

## ----nested-data--------------------------------------------------------------
d <- mixed_data("splitplot")
d$whole_plot <- interaction(d$block, d$variety, drop = TRUE)

## ----nested-model-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  nested <- mixed_nested(
    data     = d,
    response = "yield",
    fixed    = "variety * nitrogen",
    groups   = c("block", "whole_plot")
  )

  nested
}

## ----crossed-data-------------------------------------------------------------
met <- mixed_data("met")

## ----crossed-model------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  crossed <- mixed_crossed(
    data     = met,
    response = "yield",
    fixed    = "1",
    groups   = c("genotype", "environment")
  )

  crossed
}

## ----diagnose-rs--------------------------------------------------------------
if (exists("rs")) {
  mixed_diagnose(rs)
}

## ----compare-random-----------------------------------------------------------
if (exists("ri") && exists("rs")) {
  mixed_compare(
    random_intercept = ri,
    random_slope     = rs,
    method = "information"
  )
}

## ----sparse-times-------------------------------------------------------------
z2 <- subset(z, time %in% c(min(time), max(time)))

## ----sparse-slope-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rs_sparse <- try(
    mixed_random_slopes(
      data       = z2,
      response   = "height",
      fixed      = "treatment * time",
      slope      = "time",
      group      = "subject",
      correlated = TRUE
    ),
    silent = TRUE
  )

  if (inherits(rs_sparse, "try-error")) {
    cat("The engine refused to estimate this model:\n\n")
    cat(conditionMessage(attr(rs_sparse, "condition")), "\n")
  } else {
    mixed_diagnose(rs_sparse)
  }
}

## ----random-table-------------------------------------------------------------
if (exists("rs")) {
  mixed_table(
    rs,
    component = "random",
    format = "markdown",
    caption = "Random-effect variance-covariance summary"
  )
}

## ----exercise1----------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  e1a <- mixed_random_intercepts(z, "height", "time", "subject")
  e1b <- mixed_random_intercepts(z, "height", "treatment * time", "subject")
}

## ----exercise2----------------------------------------------------------------
if (exists("rs") && exists("rs_diag")) {
  mixed_compare(
    correlated   = rs,
    uncorrelated = rs_diag,
    method = "information"
  )
}

