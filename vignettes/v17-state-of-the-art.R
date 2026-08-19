## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----lme4-map-----------------------------------------------------------------
library(mixedFlowR)
mixed_blocks("LMM|GLMM|random")

## ----dharma-map---------------------------------------------------------------
mixed_blocks("diagnostic")

## ----capabilities-------------------------------------------------------------
mixed_capabilities()

## ----versions-----------------------------------------------------------------
core_pkgs <- c(
  "mixedFlowR", "lme4", "nlme", "glmmTMB", "robustlmm",
  "emmeans", "pbkrtest", "confintROB", "DHARMa", "gamlss",
  "brms", "SpATS", "sommer"
)

available_versions <- lapply(core_pkgs, function(p) {
  if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
})
names(available_versions) <- core_pkgs
available_versions

