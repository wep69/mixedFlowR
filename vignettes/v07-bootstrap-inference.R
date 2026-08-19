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

## ----model--------------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- mixed_fit(
    height ~ treatment * time + (1 | subject),
    data = z
  )
}

## ----bootstrap-demo-----------------------------------------------------------
if (exists("fit")) {
  boot_demo <- mixed_boot_fixed(
    fit,
    B = 49,
    level = 0.95,
    seed = 260818
  )

  boot_demo
}

## ----bootstrap-production, eval=FALSE-----------------------------------------
# boot_fixed <- mixed_boot_fixed(
#   fit,
#   B = 4999,
#   level = 0.95,
#   seed = 260818
# )

## ----boot-table---------------------------------------------------------------
if (exists("boot_demo")) {
  mixed_table(
    boot_demo,
    component = "bootstrap",
    format = "markdown",
    caption = "Teaching-scale parametric bootstrap intervals"
  )
}

## ----boot-plot----------------------------------------------------------------
if (exists("boot_demo")) {
  mixed_plot(
    boot_demo,
    type = "bootstrap"
  )
}

## ----variance-demo------------------------------------------------------------
if (exists("fit")) {
  boot_var_demo <- mixed_boot_variance(
    fit,
    B = 49,
    seed = 260818
  )

  boot_var_demo
}

## ----variance-production, eval=FALSE------------------------------------------
# boot_var <- mixed_boot_variance(
#   fit,
#   B = 4999,
#   seed = 260818
# )

## ----slope-fit----------------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  rs <- mixed_random_slopes(
    z,
    response = "height",
    fixed = "treatment * time",
    slope = "time",
    group = "subject"
  )
}

## ----slope-bootstrap----------------------------------------------------------
if (exists("rs")) {
  rs_var_demo <- mixed_boot_variance(
    rs,
    B = 49,
    seed = 18
  )
}

## ----pred-grid----------------------------------------------------------------
new_height <- data.frame(
  treatment = factor(c("T1", "T2"), levels = levels(z$treatment)),
  time = c(max(z$time), max(z$time)),
  subject = z$subject[1]
)
new_height

## ----pred-population----------------------------------------------------------
if (exists("fit")) {
  pred_pop_demo <- mixed_boot_predict(
    fit,
    newdata = new_height,
    B = 49,
    re.form = NA,
    seed = 260818
  )

  pred_pop_demo
}

## ----pred-conditional---------------------------------------------------------
if (exists("fit")) {
  pred_cond_demo <- mixed_boot_predict(
    fit,
    newdata = new_height,
    B = 49,
    re.form = NULL,
    seed = 260818
  )

  pred_cond_demo
}

## ----bca, eval=FALSE----------------------------------------------------------
# if (
#   requireNamespace("confintROB", quietly = TRUE) &&
#   exists("fit")
# ) {
#   boot_bca <- mixed_boot(
#     fit,
#     statistic = "fixed",
#     B = 1999,
#     method = "parametric",
#     interval = "BCa",
#     cluster = "subject",
#     seed = 260818
#   )
# }

## ----wild, eval=FALSE---------------------------------------------------------
# if (
#   requireNamespace("confintROB", quietly = TRUE) &&
#   exists("fit")
# ) {
#   boot_wild <- mixed_boot(
#     fit,
#     statistic = "fixed",
#     B = 1999,
#     method = "wild",
#     interval = "percentile",
#     cluster = "subject",
#     seed = 260818
#   )
# }

## ----robust-fit---------------------------------------------------------------
if (requireNamespace("robustlmm", quietly = TRUE)) {
  robust <- mixed_robust(
    height ~ treatment * time + (1 | subject),
    data = z
  )
}

## ----robust-boot, eval=FALSE--------------------------------------------------
# if (
#   exists("robust") &&
#   requireNamespace("confintROB", quietly = TRUE)
# ) {
#   robust_ci <- mixed_boot(
#     robust,
#     statistic = "fixed",
#     B = 1999,
#     method = "parametric",
#     interval = "BCa",
#     cluster = "subject",
#     seed = 260818
#   )
# }

## ----comparison-models--------------------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  small <- mixed_fit(
    height ~ time + (1 | subject),
    data = z
  )
  large <- mixed_fit(
    height ~ treatment + time + (1 | subject),
    data = z
  )
}

## ----pb-compare-demo----------------------------------------------------------
if (
  exists("small") && exists("large") &&
  requireNamespace("pbkrtest", quietly = TRUE)
) {
  pb_cmp_demo <- mixed_compare(
    small,
    large,
    method = "parametric-bootstrap",
    nsim = 99,
    seed = 260818
  )
  pb_cmp_demo
}

## ----pb-compare-production, eval=FALSE----------------------------------------
# pb_cmp <- mixed_compare(
#   small,
#   large,
#   method = "parametric-bootstrap",
#   nsim = 4999,
#   seed = 260818
# )

