test_that("lme4 wrapper agrees with direct lmer fixed effects", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  a <- mixed_fit(height~treatment*time+(1|subject),d,REML=FALSE)
  b <- lme4::lmer(height~treatment*time+(1|subject),data=d,REML=FALSE)
  expect_s3_class(a,"mixedflow_fit")
  expect_equal(lme4::fixef(a$model),lme4::fixef(b),tolerance=1e-8)
  expect_equal(as.numeric(logLik(a$model)),as.numeric(logLik(b)),tolerance=1e-8)
})

test_that("random slope and BLUP workflows return grouped effects", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  a <- mixed_random_slopes(d,"height","treatment*time","time","subject")
  r <- mixed_blup(a,"subject")
  expect_true(is.data.frame(r))
  expect_true(nrow(r) > 1)
})

test_that("lme4 AR1 structured component is version guarded", {
  skip_if_not_installed("lme4")
  skip_if(utils::packageVersion("lme4") < "2.0-1")
  d <- mixed_data("longitudinal")
  expect_silent(mixed_ar1(d,"height","treatment*time","time","subject",engine="lme4"))
})
