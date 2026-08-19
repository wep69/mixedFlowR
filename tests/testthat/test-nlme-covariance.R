test_that("nlme AR1 wrapper agrees with direct lme likelihood", {
  skip_if_not_installed("nlme")
  d <- mixed_data("longitudinal")
  a <- mixed_ar1(d,"height","treatment*time","time","subject",engine="nlme")
  b <- nlme::lme(height~treatment*time,random=~1|subject,correlation=nlme::corAR1(form=~time|subject),data=d)
  expect_equal(as.numeric(logLik(a$model)),as.numeric(logLik(b)),tolerance=1e-7)
})

test_that("heterogeneous residual model retains specification", {
  skip_if_not_installed("nlme")
  d <- mixed_data("longitudinal")
  a <- mixed_heterogeneity(height~treatment*time,d,~1|subject,"treatment")
  expect_equal(a$engine,"nlme")
  expect_equal(a$specification$variance_group,"treatment")
})
