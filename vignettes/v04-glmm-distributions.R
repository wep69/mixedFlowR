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

## ----binomial-data------------------------------------------------------------
library(mixedFlowR)
b <- mixed_data("binomial")
head(b)

## ----binomial-summary---------------------------------------------------------
aggregate(cbind(diseased, healthy, total) ~ treatment, data = b, FUN = sum)

## ----incidence----------------------------------------------------------------
inc_obs <- aggregate(cbind(diseased, total) ~ treatment, data = b, FUN = sum)
inc_obs$incidence <- inc_obs$diseased / inc_obs$total
inc_obs

## ----binomial-fit-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  bin_fit <- mixed_binomial(
    data      = b,
    successes = "diseased",
    failures  = "healthy",
    fixed     = "treatment",
    group     = "block",
    engine    = "lme4"
  )

  bin_fit
}

## ----binomial-emmeans---------------------------------------------------------
if (
  exists("bin_fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_emmeans(
    bin_fit,
    specs = ~ treatment,
    type = "response"
  )
}

## ----count-data---------------------------------------------------------------
cnt <- mixed_data("counts")
head(cnt)

## ----count-summary------------------------------------------------------------
aggregate(count ~ treatment, data = cnt, FUN = mean)
aggregate(I(count == 0) ~ treatment, data = cnt, FUN = mean)

## ----poisson-fit--------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  pois <- mixed_count(
    count ~ treatment * time + (1 | plot),
    data   = cnt,
    family = "poisson",
    engine = "glmmTMB"
  )

  pois
}

## ----nb2-fit------------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  nb2 <- mixed_count(
    count ~ treatment * time + (1 | plot),
    data   = cnt,
    family = "nbinom2"
  )
}

## ----nb1-fit------------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  nb1 <- mixed_count(
    count ~ treatment * time + (1 | plot),
    data   = cnt,
    family = "nbinom1"
  )
}

## ----dharma-nb2---------------------------------------------------------------
if (
  exists("nb2") &&
  requireNamespace("DHARMa", quietly = TRUE)
) {
  nb_diag <- mixed_diagnose(
    nb2,
    simulate = TRUE,
    nsim = 500
  )

  nb_diag
}

## ----zi-fit-------------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  zi <- mixed_zero_inflated(
    count ~ treatment * time + (1 | plot),
    data        = cnt,
    family      = glmmTMB::nbinom2,
    ziformula   = ~ treatment,
    dispformula = ~ 1
  )

  zi
}

## ----count-compare------------------------------------------------------------
if (exists("pois") && exists("nb1") && exists("nb2")) {
  mixed_compare(
    Poisson = pois,
    NB1     = nb1,
    NB2     = nb2,
    method  = "information"
  )
}

## ----zi-compare---------------------------------------------------------------
if (exists("nb2") && exists("zi")) {
  mixed_compare(
    NB2 = nb2,
    ZINB = zi,
    method = "information"
  )
}

## ----beta-data----------------------------------------------------------------
be <- mixed_data("beta")
summary(be$severity)
range(be$severity)

## ----beta-fit-----------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  beta_fit <- mixed_beta(
    severity ~ treatment + (1 | block),
    data = be,
    link = "logit"
  )

  beta_fit
}

## ----beta-emm-----------------------------------------------------------------
if (
  exists("beta_fit") &&
  requireNamespace("emmeans", quietly = TRUE)
) {
  mixed_emmeans(
    beta_fit,
    specs = ~ treatment,
    type = "response"
  )
}

## ----beta-boundary, eval=FALSE------------------------------------------------
# be_bad <- be
# be_bad$severity[1] <- 0
# mixed_beta(severity ~ treatment + (1 | block), be_bad)

## ----generic-glmm-------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  generic_bin <- mixed_glmm(
    cbind(diseased, healthy) ~ treatment + (1 | block),
    data   = b,
    family = stats::binomial(),
    engine = "lme4"
  )
}

## ----count-raw-plot-----------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(cnt, ggplot2::aes(time, count, colour = treatment)) +
    ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.55) +
    ggplot2::stat_summary(fun = mean, geom = "line", ggplot2::aes(group = treatment), linewidth = 0.8) +
    ggplot2::labs(
      x = "Time",
      y = "Pest count",
      colour = "Treatment",
      title = "Observed count trajectories"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

## ----exercise-zi--------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  zi_intercept <- mixed_zero_inflated(
    count ~ treatment * time + (1 | plot),
    data = cnt,
    family = glmmTMB::nbinom2,
    ziformula = ~ 1
  )
}

## ----exercise-beta------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  beta_logit <- mixed_beta(severity ~ treatment + (1 | block), be, "logit")
  beta_probit <- mixed_beta(severity ~ treatment + (1 | block), be, "probit")
}

