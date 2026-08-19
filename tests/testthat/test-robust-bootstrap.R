test_that("robust sensitivity comparison does not delete data", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("robustlmm")
  d <- mixed_data("longitudinal")
  d$height[1] <- d$height[1] + 20
  classical <- mixed_fit(height~time+(1|subject),d)
  robust <- mixed_robust(height~time+(1|subject),d)
  z <- mixed_compare_robust(classical,robust)
  expect_true(all(c("estimate.classical","estimate.robust","shift") %in% names(z)))
  flags <- mixed_contamination(classical)
  expect_equal(nrow(flags),nrow(d))
})

test_that("small parametric bootstrap returns stored replicates", {
  skip_if_not_installed("lme4")
  d <- mixed_data("longitudinal")
  m <- mixed_fit(height~time+(1|subject),d)
  b <- mixed_boot_fixed(m,B=5,seed=260818)
  expect_s3_class(b,"mixedflow_boot")
  expect_equal(nrow(b$t),5L)
})
