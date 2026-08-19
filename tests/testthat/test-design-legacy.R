test_that("design audit detects balance loss", {
  d <- mixed_data("splitplot")
  a <- mixed_design_audit(d, "yield", block="block", whole="variety", sub="nitrogen")
  expect_s3_class(a, "mixedflow_audit")
  expect_true(all(a$balance$n > 0))
  a2 <- mixed_design_audit(d[-1,], "yield", block="block", whole="variety", sub="nitrogen")
  expect_true(any(a2$balance$n == 0))
})

test_that("legacy split-plot exposes distinct error strata", {
  d <- mixed_data("splitplot")
  a <- fit_legacy_splitplot(d,"yield","block","variety","nitrogen")
  expect_true(inherits(a,"aovlist"))
  e <- splitplot_ems(d,"block","variety","nitrogen")
  expect_true(all(c("source","df","denominator") %in% names(e)))
  expect_true(any(grepl("whole", e$denominator, ignore.case=TRUE)))
  tab <- splitplot_anova(a)
  expect_true(is.data.frame(tab))
})
