test_that("glmmTMB count and zero-inflated branches fit", {
  skip_if_not_installed("glmmTMB")
  d <- mixed_data("counts")
  a <- mixed_count(count~treatment*time+(1|plot),d,"nbinom2")
  expect_equal(a$engine,"glmmTMB")
  z <- mixed_zero_inflated(count~treatment*time+(1|plot),d,family=glmmTMB::nbinom2(),ziformula=~treatment)
  expect_equal(z$engine,"glmmTMB")
})

test_that("continuous OU and ordered AR1 use distinct time encodings", {
  skip_if_not_installed("glmmTMB")
  d <- mixed_data("longitudinal")
  ou <- mixed_spatiotemporal(d,"height","time","time","subject","ou")
  ar <- mixed_spatiotemporal(d,"height","time","time","subject","ar1")
  expect_true(is.factor(ou$data$.mf_time))
  expect_true(is.ordered(ar$data$.mf_time))
})
