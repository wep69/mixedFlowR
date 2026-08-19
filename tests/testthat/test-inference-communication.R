test_that("quantitative predictors remain quantitative", {
  # WHY: the point of this test is that a dose is modelled as a dose. lme4 emits a
  # scale caution here because nitrogen is measured in kg/ha (0 to 150) while the
  # other terms are on unit scale. The caution is expected and is NOT silenced by
  # rescaling: the agronomic reading of the coefficients depends on the natural
  # units of the applied rate, and the polynomial basis is fitted on that scale.
  skip_if_not_installed("lme4")
  d <- mixed_data("splitplot")
  m <- suppressWarnings(
    mixed_quantitative(d, "yield", "nitrogen", "variety", "block", degree = 2, basis = "polynomial")
  )
  expect_match(paste(deparse(formula(m$model)), collapse = " "), "poly")
})

test_that("EMM trends and curves are available when emmeans is installed", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")
  d <- mixed_data("splitplot")
  m <- mixed_fit(yield~variety*nitrogen+(1|block)+(1|block:variety),d)
  tr <- mixed_trend(m,~variety,"nitrogen")
  expect_s4_class(tr,"emmGrid")
  cv <- mixed_curve(m,"nitrogen",c(0,50,100,150),"variety")
  expect_true(is.data.frame(cv))
})

test_that("publication communication returns reusable objects", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  m <- mixed_fit(height~time+(1|subject),d)
  expect_s3_class(mixed_table(m,"fixed"),"mixedflow_table")
  expect_s3_class(mixed_plot(m,"raw",x="time"),"ggplot")
  expect_s3_class(mixed_plot(m,"residuals"),"ggplot")
})
