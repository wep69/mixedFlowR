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

## ----classical----------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  classical <- mixed_fit(
    height ~ treatment * time + (1 | subject),
    data = z
  )
}

## ----robust-------------------------------------------------------------------
if (requireNamespace("robustlmm", quietly = TRUE)) {
  robust <- mixed_robust(
    height ~ treatment * time + (1 | subject),
    data = z
  )

  robust
}

## ----compare------------------------------------------------------------------
if (exists("classical") && exists("robust")) {
  robust_shift <- mixed_compare_robust(
    classical,
    robust
  )

  robust_shift
}

## ----contaminate--------------------------------------------------------------
z_cont <- z
z_cont$height[1] <- z_cont$height[1] + 30

## ----contamination-plot-------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(z_cont, ggplot2::aes(time, height, group = subject, colour = treatment)) +
    ggplot2::geom_line(alpha = 0.25) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = "Teaching contamination scenario")
}

## ----classical-cont-----------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  classical_cont <- mixed_fit(
    height ~ treatment * time + (1 | subject),
    data = z_cont
  )
}

## ----robust-cont--------------------------------------------------------------
if (requireNamespace("robustlmm", quietly = TRUE)) {
  robust_cont <- mixed_robust(
    height ~ treatment * time + (1 | subject),
    data = z_cont
  )
}

## ----compare-cont-------------------------------------------------------------
if (exists("classical_cont") && exists("robust_cont")) {
  mixed_compare_robust(
    classical_cont,
    robust_cont
  )
}

## ----influence-observation----------------------------------------------------
if (exists("classical_cont")) {
  obs_screen <- mixed_robust_influence(
    classical_cont
  )

  head(obs_screen[order(-obs_screen$abs_robust_z), ])
}

## ----influence-cluster--------------------------------------------------------
if (exists("classical_cont")) {
  cluster_screen <- mixed_robust_influence(
    classical_cont,
    cluster = "subject"
  )

  cluster_screen
}

## ----contamination-screen-----------------------------------------------------
if (exists("classical_cont")) {
  flags <- mixed_contamination(
    classical_cont,
    cluster   = "subject",
    threshold = 2.5
  )

  flags
}

## ----cr2----------------------------------------------------------------------
if (
  exists("classical") &&
  requireNamespace("clubSandwich", quietly = TRUE)
) {
  V_CR2 <- mixed_robust_vcov(
    classical,
    cluster = "subject",
    type    = "CR2"
  )

  V_CR2
}

## ----influence-me-------------------------------------------------------------
if (
  exists("classical") &&
  requireNamespace("influence.ME", quietly = TRUE)
) {
  infl <- mixed_influence(
    classical,
    group = "subject"
  )

  infl
}

## ----robust-table-------------------------------------------------------------
if (exists("robust_cont")) {
  mixed_table(
    robust_cont,
    component = "fixed",
    format = "markdown",
    caption = "Robust mixed-model fixed-effect estimates"
  )
}

## ----strict-threshold---------------------------------------------------------
if (exists("classical_cont")) {
  mixed_contamination(
    classical_cont,
    cluster = "subject",
    threshold = 3
  )
}

## ----exercise-cr--------------------------------------------------------------
if (
  exists("classical") &&
  requireNamespace("clubSandwich", quietly = TRUE)
) {
  V_CR1 <- mixed_robust_vcov(classical, "subject", "CR1")
  V_CR2 <- mixed_robust_vcov(classical, "subject", "CR2")
}

