test_that("GAMLSS parameter extraction is row-stable", {
  skip_if_not_installed("gamlss")
  d <- mixed_data("longitudinal")
  m <- mixed_gamlss(height~time,d,family="NO",sigma.formula=~treatment)
  p <- mixed_gamlss_parameters(m)
  expect_equal(nrow(p),nrow(d))
  expect_true(all(c("row","mu","sigma") %in% names(p)))
})

test_that("Bayesian prior inspection does not require sampling", {
  skip_if_not_installed("brms")
  d <- mixed_data("longitudinal")
  p <- mixed_prior(height~time+(1|subject),d)
  expect_true(is.data.frame(p))
})

test_that("SpATS and sommer adapters are guarded and typed", {
  if (requireNamespace("SpATS",quietly=TRUE)) {
    d <- mixed_data("spatial")
    s <- mixed_spatial_field(d,"yield","genotype","row","col","block",nseg=c(5,5))
    expect_equal(s$engine,"SpATS")
  }
  if (requireNamespace("sommer",quietly=TRUE)) {
    d <- mixed_data("met")
    m <- mixed_met(yield~environment,~genotype,d)
    expect_equal(m$engine,"sommer")
  }
  succeed()
})

test_that("spatial design terms accept column names and formulas equivalently", {
  # WHY: blocks and replicates are design nuisance terms, so the string-first idiom
  # used by the rest of the package ("block") must reach SpATS as ~ block instead of
  # as an invalid formula. Before this guard the call died inside the backend with
  # 'invalid formula "block": not a call', which hides the scientific meaning.
  skip_if_not_installed("SpATS")
  d <- mixed_data("spatial")
  by_name <- mixed_spatial_field(d, "yield", "genotype", "row", "col", "block", nseg = c(5, 5))
  by_formula <- mixed_spatial_field(d, "yield", "genotype", "row", "col", ~block, nseg = c(5, 5))
  expect_equal(by_name$engine, "SpATS")
  expect_equal(stats::fitted(by_name$model), stats::fitted(by_formula$model), tolerance = 1e-8)
})

test_that("an absent design column is refused with the scientific reason", {
  # WHY: a design term that does not exist in the experiment is a design error, not a
  # numerical one, so it must be reported before any estimation starts.
  d <- mixed_data("spatial")
  expect_error(mixedFlowR:::.mf_rhs_formula("bloco", d, "random"),
               "absent from the data")
  expect_error(mixedFlowR:::.mf_rhs_formula(yield ~ block, d, "fixed"),
               "without a response")
})
