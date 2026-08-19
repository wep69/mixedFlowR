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

## ----prior-classes------------------------------------------------------------
if (requireNamespace("brms", quietly = TRUE)) {
  prior_classes <- mixed_prior(
    height ~ treatment * time + (time | subject),
    data = z
  )

  prior_classes
}

## ----explicit-priors, eval=FALSE----------------------------------------------
# priors <- c(
#   brms::prior(normal(20, 10), class = "Intercept"),
#   brms::prior(normal(0, 3), class = "b"),
#   brms::prior(exponential(1), class = "sd"),
#   brms::prior(exponential(1), class = "sigma")
# )

## ----prior-predictive, eval=FALSE---------------------------------------------
# prior_fit <- mixed_prior_predictive(
#   height ~ treatment * time + (time | subject),
#   data   = z,
#   prior  = priors,
#   chains = 2,
#   iter   = 1000,
#   seed   = 260818
# )

## ----bayes-fit, eval=FALSE----------------------------------------------------
# bfit <- mixed_bayes(
#   height ~ treatment * time + (time | subject),
#   data   = z,
#   prior  = priors,
#   chains = 4,
#   iter   = 4000,
#   seed   = 260818
# )

## ----posterior-draws, eval=FALSE----------------------------------------------
# draws <- mixed_posterior(bfit)
# draws

## ----posterior-selected, eval=FALSE-------------------------------------------
# time_draws <- mixed_posterior(
#   bfit,
#   variables = "b_time"
# )

## ----posterior-probability, eval=FALSE----------------------------------------
# d <- mixed_posterior(bfit, variables = "b_treatmentT2")
# mean(d$b_treatmentT2 > 0)
# mean(d$b_treatmentT2 > 2)

## ----ppc-density, eval=FALSE--------------------------------------------------
# mixed_pp_check(
#   bfit,
#   type = "dens_overlay",
#   ndraws = 100
# )

## ----ppc-ecdf, eval=FALSE-----------------------------------------------------
# mixed_pp_check(
#   bfit,
#   type = "ecdf_overlay",
#   ndraws = 100
# )

## ----loo-single, eval=FALSE---------------------------------------------------
# loo_fit <- mixed_loo(bfit)
# loo_fit

## ----loo-compare, eval=FALSE--------------------------------------------------
# b0 <- mixed_bayes(
#   height ~ time + (time | subject),
#   data = z,
#   prior = priors,
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )
# 
# mixed_loo(b0, bfit)

## ----bayes-binomial, eval=FALSE-----------------------------------------------
# b <- mixed_data("binomial")
# 
# b_bin <- mixed_bayes(
#   diseased | trials(total) ~ treatment + (1 | block),
#   data = b,
#   family = binomial(),
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )

## ----distributional, eval=FALSE-----------------------------------------------
# bf_dist <- brms::bf(
#   height ~ treatment * time + (1 | subject),
#   sigma ~ treatment
# )
# 
# b_dist <- mixed_bayes(
#   bf_dist,
#   data = z,
#   prior = distributional_priors,  # define scientifically justified priors before fitting
#   chains = 4,
#   iter = 4000,
#   seed = 260818
# )

