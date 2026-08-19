## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----data---------------------------------------------------------------------
library(mixedFlowR)
m <- mixed_data("met")
head(m)

## ----support------------------------------------------------------------------
with(m, table(environment, rep))
with(m, table(environment, genotype))

## ----environment-means--------------------------------------------------------
aggregate(yield ~ environment, m, mean)

## ----genotype-means-----------------------------------------------------------
gm <- aggregate(yield ~ genotype, m, mean)
gm[order(-gm$yield), ][1:8, ]

## ----profile-plot, fig.height=5.5---------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  prof <- aggregate(yield ~ genotype + environment, m, mean)

  ggplot2::ggplot(
    prof,
    ggplot2::aes(environment, yield, group = genotype, colour = genotype)
  ) +
    ggplot2::geom_line(alpha = 0.55) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(
      x = "Environment",
      y = "Mean yield",
      colour = "Genotype",
      title = "Observed genotype profiles across environments"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}

## ----baseline-met-------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  met_fit <- mixed_met(
    fixed  = yield ~ environment,
    random = ~ genotype,
    data   = m,
    solver = "mmes"
  )

  met_fit
}

## ----random-env---------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  met_random_env <- mixed_met(
    fixed  = yield ~ 1,
    random = ~ genotype + environment,
    data   = m
  )
}

## ----diagonal-----------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  gxe_diag <- mixed_gxe(
    data        = m,
    response    = "yield",
    genotype    = "genotype",
    environment = "environment",
    model       = "diagonal"
  )

  gxe_diag
}

## ----unstructured-------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  gxe_us <- mixed_gxe(
    m,
    response = "yield",
    genotype = "genotype",
    environment = "environment",
    model = "unstructured"
  )
}

## ----fa2----------------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  gxe_fa2 <- mixed_gxe(
    data        = m,
    response    = "yield",
    genotype    = "genotype",
    environment = "environment",
    model       = "fa",
    nPC         = 2
  )
}

## ----fa1----------------------------------------------------------------------
if (requireNamespace("sommer", quietly = TRUE)) {
  gxe_fa1 <- mixed_gxe(
    m,
    "yield",
    "genotype",
    "environment",
    model = "fa",
    nPC = 1
  )
}

## ----crossed-baseline---------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  crossed <- mixed_crossed(
    data = m,
    response = "yield",
    fixed = "1",
    groups = c("genotype", "environment")
  )
}

## ----support-audit------------------------------------------------------------
mixed_balance(m, c("environment", "rep", "genotype"))

## ----missing-met--------------------------------------------------------------
m_missing <- m[-1, ]
mixed_balance(m_missing, c("environment", "rep", "genotype"))

## ----genomic-template, eval=FALSE---------------------------------------------
# # Define `genomic_random_formula` from a scientifically justified
# # sommer relationship structure before fitting.
# fit_genomic <- mixed_met(
#   fixed = yield ~ environment,
#   random = genomic_random_formula,
#   data = met_with_relationships,
#   solver = "mmes"
# )

