## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----capabilities-------------------------------------------------------------
library(mixedFlowR)
mixed_capabilities()

## ----blocks-------------------------------------------------------------------
blocks <- mixed_blocks()
blocks
nrow(blocks)

## ----datasets-----------------------------------------------------------------
names_to_check <- c(
  "splitplot", "longitudinal", "counts", "binomial", "beta",
  "spatial", "met", "growth", "dose_response"
)

lapply(names_to_check, function(x) dim(mixed_data(x)))

## ----seed---------------------------------------------------------------------
set.seed(260818)
rnorm(5)
set.seed(260818)
rnorm(5)

## ----golden-example, eval=FALSE-----------------------------------------------
# d <- mixed_data("longitudinal")
# 
# wrapped <- mixed_fit(
#   height ~ treatment*time + (1|subject),
#   d,
#   engine = "lme4"
# )
# 
# direct <- lme4::lmer(
#   height ~ treatment*time + (1|subject),
#   data = d
# )
# 
# testthat::expect_equal(
#   lme4::fixef(wrapped$model),
#   lme4::fixef(direct),
#   tolerance = 1e-8
# )

## ----legacy-benchmark---------------------------------------------------------
d <- mixed_data("splitplot")
d$nitrogen_f <- factor(d$nitrogen)

legacy <- fit_legacy_splitplot(
  d, "yield", "block", "variety", "nitrogen_f"
)
splitplot_anova(legacy)

## ----legacy-mixed-benchmark---------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  compare_legacy_mixed(
    d, "yield", "block", "variety", "nitrogen_f"
  )
}

## ----testthat, eval=FALSE-----------------------------------------------------
# testthat::test_local()

## ----testthat-reporter, eval=FALSE--------------------------------------------
# testthat::test_local(reporter = "progress")

## ----roxygen, eval=FALSE------------------------------------------------------
# roxygen2::roxygenise()

## ----load-all, eval=FALSE-----------------------------------------------------
# pkgload::load_all()

## ----rcmdcheck, eval=FALSE----------------------------------------------------
# rcmdcheck::rcmdcheck(
#   args = c("--as-cran"),
#   error_on = "warning"
# )

## ----session-info-------------------------------------------------------------
utils::sessionInfo()

## ----package-versions---------------------------------------------------------
pkgs <- c("mixedFlowR", "lme4", "nlme", "glmmTMB", "emmeans")
sapply(pkgs, function(p) {
  if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
})

