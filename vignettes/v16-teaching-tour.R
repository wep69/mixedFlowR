## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----tours--------------------------------------------------------------------
library(mixedFlowR)

topics <- c(
  "splitplot",
  "random_effects",
  "covariance",
  "robust",
  "gamlss",
  "bayes",
  "spatial",
  "met"
)

lapply(topics, mixed_tour)

## ----splitplot-tour-----------------------------------------------------------
tour_split <- mixed_tour("splitplot")
tour_split$steps

## ----splitplot-run------------------------------------------------------------
tour_split_run <- mixed_tour("splitplot", run = TRUE)
tour_split_run$artifact

## ----random-tour--------------------------------------------------------------
mixed_tour("random_effects")$steps

## ----random-demo--------------------------------------------------------------
z <- mixed_data("longitudinal")

if (requireNamespace("lme4", quietly = TRUE)) {
  ri <- mixed_random_intercepts(z, "height", "treatment*time", "subject")
  rs <- mixed_random_slopes(z, "height", "treatment*time", "time", "subject")

  mixed_plot(rs, type = "random")
}

## ----covariance-tour----------------------------------------------------------
mixed_tour("covariance")$steps

## ----covariance-demo----------------------------------------------------------
if (requireNamespace("nlme", quietly = TRUE)) {
  ar1 <- mixed_ar1(z, "height", "treatment*time", "time", "subject")
  het <- mixed_heterogeneity(
    height ~ treatment * time,
    z,
    random = ~ 1 | subject,
    variance_group = "treatment"
  )
}

## ----robust-tour--------------------------------------------------------------
mixed_tour("robust")$steps

## ----robust-demo--------------------------------------------------------------
z_cont <- z
z_cont$height[1] <- z_cont$height[1] + 30

if (
  requireNamespace("lme4", quietly = TRUE) &&
  requireNamespace("robustlmm", quietly = TRUE)
) {
  classical <- mixed_fit(height ~ treatment*time + (1|subject), z_cont)
  robust <- mixed_robust(height ~ treatment*time + (1|subject), z_cont)
  mixed_compare_robust(classical, robust)
}

## ----gamlss-tour--------------------------------------------------------------
mixed_tour("gamlss")$steps

## ----gamlss-demo--------------------------------------------------------------
if (requireNamespace("gamlss", quietly = TRUE)) {
  g_scale <- mixed_gamlss(
    height ~ treatment*time,
    z,
    family = "NO",
    sigma.formula = ~ treatment,
    random = "subject"
  )

  head(mixed_gamlss_parameters(g_scale, c("mu", "sigma")))
}

## ----bayes-tour---------------------------------------------------------------
mixed_tour("bayes")$steps

## ----bayes-prior--------------------------------------------------------------
if (requireNamespace("brms", quietly = TRUE)) {
  mixed_prior(
    height ~ treatment*time + (time|subject),
    data = z
  )
}

## ----bayes-full, eval=FALSE---------------------------------------------------
# bfit <- mixed_bayes(
#   height ~ treatment*time + (time|subject),
#   z,
#   prior = priors,
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )
# 
# mixed_pp_check(bfit)

## ----spatial-tour-------------------------------------------------------------
mixed_tour("spatial")$steps

## ----spatial-demo-------------------------------------------------------------
s <- mixed_data("spatial")

if (requireNamespace("SpATS", quietly = TRUE)) {
  sp <- mixed_spatial_field(s, "yield", "genotype", "col", "row", random = ~ block)
  mixed_plot(sp, type = "spatial")
}

## ----met-tour-----------------------------------------------------------------
mixed_tour("met")$steps

## ----met-demo-----------------------------------------------------------------
m <- mixed_data("met")

if (requireNamespace("sommer", quietly = TRUE)) {
  gxe <- mixed_gxe(m, "yield", "genotype", "environment", "fa", nPC = 2)
}

