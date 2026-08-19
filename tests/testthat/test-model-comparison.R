test_that("different fixed effects are not LRT-compared under REML", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  a <- mixed_fit(height~time+(1|subject),d,REML=TRUE)
  b <- mixed_fit(height~treatment+time+(1|subject),d,REML=TRUE)
  expect_error(mixed_compare(a,b,method="LRT"),"require ML")
})

test_that("information comparison does not declare an automatic winner", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  a <- mixed_fit(height~time+(1|subject),d,REML=FALSE)
  b <- mixed_fit(height~treatment+time+(1|subject),d,REML=FALSE)
  z <- mixed_compare(a,b)
  expect_true(all(c("AIC","BIC","logLik") %in% names(z)))
  expect_false("winner" %in% names(z))
})
