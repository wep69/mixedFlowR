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
d <- mixed_data("splitplot")
head(d)

## ----counts-------------------------------------------------------------------
with(d, table(block, variety))
with(d, table(variety, nitrogen))

## ----audit--------------------------------------------------------------------
audit <- mixed_design_audit(
  d,
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
support <- mixed_balance(d, c("block", "variety", "nitrogen"))
support

## ----wholeplot-id-------------------------------------------------------------
d$whole_plot <- interaction(d$block, d$variety, drop = TRUE)
length(unique(d$whole_plot))
nrow(d)

## ----pseudo-------------------------------------------------------------------
mixed_pseudorep(
  d,
  response  = "yield",
  cluster   = "whole_plot",
  treatment = "variety"
)

## ----factor-dose--------------------------------------------------------------
d$nitrogen_f <- factor(d$nitrogen)

## ----legacy-------------------------------------------------------------------
legacy <- fit_legacy_splitplot(
  d,
  response = "yield",
  block    = "block",
  whole    = "variety",
  sub      = "nitrogen_f"
)
legacy

## ----legacy-anova-------------------------------------------------------------
legacy_tab <- splitplot_anova(legacy)
legacy_tab

## ----ems----------------------------------------------------------------------
ems <- splitplot_ems(
  d,
  block = "block",
  whole = "variety",
  sub   = "nitrogen_f"
)
ems

## ----mixed-equivalent---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_factor <- mixed_fit(
    yield ~ variety * nitrogen_f +
      (1 | block) +
      (1 | block:variety),
    data = d
  )

  fit_factor
}

## ----compare------------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  compare_legacy_mixed(
    d,
    "yield",
    "block",
    "variety",
    "nitrogen_f"
  )
}

## ----numeric-dose-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit_numeric <- mixed_fit(
    yield ~ variety * poly(nitrogen, 2) +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

## ----numeric-curve------------------------------------------------------------
if (
  exists("fit_numeric") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_plot(
    fit_numeric,
    type      = "curve",
    x         = "nitrogen",
    by        = "variety",
    values    = seq(min(d$nitrogen), max(d$nitrogen), length.out = 61),
    show_data = TRUE
  )
}

## ----incomplete---------------------------------------------------------------
d_incomplete <- d[-1, ]
mixed_balance(d_incomplete, c("block", "variety", "nitrogen"))

## ----splitsplit-data----------------------------------------------------------
d_ss <- d
d_ss$stage <- factor(rep(c("S1", "S2"), length.out = nrow(d_ss)))

## ----splitsplit-fit-----------------------------------------------------------
ss <- fit_legacy_splitsplit(
  d_ss,
  response = "yield",
  block    = "block",
  whole    = "variety",
  sub      = "nitrogen_f",
  subsub   = "stage"
)
ss

## ----strip-data---------------------------------------------------------------
d_strip <- d
d_strip$A <- factor(d_strip$variety)
d_strip$B <- factor(d_strip$nitrogen)

## ----strip-fit----------------------------------------------------------------
strip <- fit_legacy_stripplot(
  d_strip,
  response = "yield",
  block    = "block",
  strip_a  = "A",
  strip_b  = "B"
)
strip

## ----repeated-data------------------------------------------------------------
z <- mixed_data("longitudinal")
head(z)

## ----repeated-audit-----------------------------------------------------------
mixed_design_audit(
  z,
  response = "height",
  subject  = "subject",
  time     = "time"
)

## ----wrong-model--------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  wrong <- mixed_fit(
    yield ~ variety * nitrogen_f + (1 | block),
    data = d
  )

  correct <- mixed_fit(
    yield ~ variety * nitrogen_f +
      (1 | block) +
      (1 | block:variety),
    data = d
  )
}

