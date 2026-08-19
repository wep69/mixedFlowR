## Regression guards for defects found when the tutorials were actually executed.
## Each block records WHY the behaviour matters, not only that it works.

test_that("small-sample fixed-effect tests survive a fit made inside a function", {
  # WHY: lmerTest and pbkrtest REBUILD the model from its recorded call. A wrapper
  # that records data = <internal variable name> makes Satterthwaite and
  # Kenward-Roger impossible, which silently removes the small-sample inference
  # that agronomic split-plot experiments depend on.
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  d <- mixed_data("splitplot")
  fit <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), d)
  sat <- mixed_anova(fit, ddf = "Satterthwaite")
  expect_s3_class(sat, "data.frame")
  expect_true(nrow(sat) >= 3L)
  skip_if_not_installed("pbkrtest")
  kr <- mixed_anova(fit, ddf = "Kenward-Roger")
  expect_s3_class(kr, "data.frame")
  expect_true(all(is.finite(kr[["DenDF"]])))
})

test_that("Kenward-Roger refuses an ML fit and says why", {
  # WHY: the correction uses the REML covariance of the variance components, so an
  # ML fit cannot supply it. The user must learn the reason, not read a backend
  # message about deviance functions.
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("pbkrtest")
  z <- mixed_data("longitudinal")
  ml <- mixed_fit(height ~ treatment + time + (1 | subject), z, REML = FALSE)
  expect_error(mixed_anova(ml, ddf = "Kenward-Roger"), "REML")
})

test_that("parametric-bootstrap model comparison can refit the model", {
  # WHY: PBmodcomp simulates from one model and refits both. With a non
  # re-evaluable call it used to find utils::data instead of the experiment.
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  z <- mixed_data("longitudinal")
  m0 <- mixed_fit(height ~ time + (1 | subject), z, REML = FALSE)
  m1 <- mixed_fit(height ~ treatment + time + (1 | subject), z, REML = FALSE)
  cmp <- mixed_compare(m0, m1, method = "parametric-bootstrap", nsim = 9, seed = 1)
  expect_true(is.data.frame(cmp) || is.list(cmp))
})

test_that("influence diagnostics work through the legacy influence.ME interface", {
  # WHY: influence.ME does not evaluate the model call; it reads the third element
  # of the call as text and looks it up with get(). The adapter must use the name
  # that influence.ME itself reserves for model-frame data.
  skip_if_not_installed("lme4")
  skip_if_not_installed("influence.ME")
  z <- mixed_data("longitudinal")
  m <- mixed_fit(height ~ treatment + time + (1 | subject), z)
  inf <- mixed_influence(m, "subject")
  expect_s3_class(inf, "estex")
})

test_that("a design audit works when only repeated measurements are declared", {
  # WHY: a longitudinal design has no whole-plot or subplot treatment to pair with
  # the cluster. Screening the cluster support must still be possible, otherwise
  # the audit refuses exactly the design it is meant to describe.
  z <- mixed_data("longitudinal")
  a <- mixed_design_audit(z, "height", subject = "subject", time = "time")
  expect_s3_class(a, "mixedflow_audit")
  ps <- mixed_pseudorep(z, "height", cluster = "subject")
  expect_true(ps$cluster_declared)
  expect_equal(sum(ps$counts$n), nrow(z))
})

test_that("bootstrap intervals carry the name of the parameter they belong to", {
  # WHY: an interval without its parameter cannot be reported. The table used to
  # fail outright because the interval matrix had no row names.
  skip_if_not_installed("lme4")
  z <- mixed_data("longitudinal")
  m <- mixed_fit(height ~ treatment + time + (1 | subject), z)
  b <- mixed_boot_fixed(m, B = 19, seed = 260818)
  expect_false(is.null(rownames(b$confint)))
  tab <- mixed_table(b, component = "bootstrap")
  expect_true(all(c("parameter", "lower", "upper") %in% names(tab)))
  expect_equal(nrow(tab), length(lme4::fixef(m$model)))
})

test_that("GxE covariance models run and reduced rank refuses an incomplete grid", {
  # WHY: the reduced-rank model needs a complete genotype-by-environment table of
  # observed means. A missing combination is a design gap and must be reported as
  # such instead of being imputed silently inside the backend.
  skip_if_not_installed("sommer")
  d <- mixed_data("met")
  expect_equal(mixed_gxe(d, "yield", "genotype", "environment", "diagonal")$engine, "sommer")
  expect_equal(mixed_gxe(d, "yield", "genotype", "environment", "fa", nPC = 2)$engine, "sommer")
  expect_error(mixed_gxe(d, "yield", "genotype", "environment", "fa", nPC = 99),
               "exceeds")
  gap <- d[!(d$genotype == levels(d$genotype)[1] & d$environment == levels(d$environment)[1]), ]
  expect_error(mixed_gxe(gap, "yield", "genotype", "environment", "fa", nPC = 2),
               "every environment")
})

test_that("the GAMLSS random-effect routes both estimate", {
  # WHY: gamlss resolves re() through the search path and reads its data argument
  # in the global environment. Both random-effect routes must therefore be usable
  # from inside the package, otherwise the distributional module loses the only
  # way of representing repeated measurements.
  skip_if_not_installed("gamlss")
  z <- mixed_data("longitudinal")
  expect_equal(mixed_gamlss(height ~ time, data = z, family = "NO", random = "subject")$engine, "gamlss")
  skip_if_not_installed("nlme")
  expect_equal(mixed_gamlss(height ~ treatment * time, data = z, family = "NO",
                            re_random = ~1 | subject)$engine, "gamlss")
})

test_that("spatial field trials accept design terms as names or formulas", {
  # WHY: see test-gamlss-bayes-specialized.R; kept here as the adapter contract.
  skip_if_not_installed("SpATS")
  s <- mixed_data("spatial")
  expect_equal(mixed_spatial_field(s, "yield", "genotype", "col", "row", random = "block",
                                   nseg = c(5, 5))$engine, "SpATS")
})
