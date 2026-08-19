# mixedFlowR

`mixedFlowR` is a design-aware R workflow for mixed models in agronomy and related experimental sciences. It links classical experimental-error strata to modern hierarchical, distributional, robust, Bayesian, nonlinear, spatial, longitudinal and multi-environment analyses while retaining the scientific design and the backend model object.

## Core workflow

```r
library(mixedFlowR)

d <- mixed_data("splitplot")
audit <- mixed_design_audit(
  d, response = "yield", block = "block",
  whole = "variety", sub = "nitrogen"
)

legacy <- fit_legacy_splitplot(
  d, response = "yield", block = "block",
  whole = "variety", sub = "nitrogen"
)

fit <- mixed_fit(
  yield ~ variety * nitrogen + (1 | block) + (1 | block:variety),
  d
)

diag <- mixed_diagnose(fit)
tab <- mixed_table(fit, component = "fixed")
fig <- mixed_plot(fit, type = "residuals")
```

## Quantitative factors are not forced into post-hoc groups

```r
trend <- mixed_trend(fit, ~ variety, var = "nitrogen")
curve <- mixed_curve(fit, "nitrogen", seq(0, 150, by = 10), by = "variety")
```

## Advanced engines

The 70-block workflow covers:

- legacy split-plot, split-split plot and strip-plot error strata;
- LMM, GLMM, random intercepts/slopes, nesting and crossing;
- heterogeneous residuals, AR(1), continuous AR(1), Toeplitz, OU and spatial covariance;
- robust LMMs, contamination sensitivity and cluster-robust covariance;
- parametric, wild and BCa bootstrap workflows;
- mixed GAMLSS with location, scale and shape modelling;
- Bayesian multilevel and distributional workflows through `brms`;
- EMMs, contrasts, trends and curves for qualitative and quantitative factors;
- nonlinear growth, dose-response and random-regression models;
- P-spline spatial field trials through `SpATS`;
- multi-environment and reduced-rank GxE models through `sommer`;
- diagnostics, publication-oriented tables and 600-dpi/vector-ready plots;
- guided teaching tours connecting classical designs to modern mixed-model reasoning.

Heavy modelling engines are optional. `mixedFlowR` fails explicitly when a requested backend is unavailable and does not silently simplify the scientific model.

Use `mixed_blocks()` to inspect the 70 implementation blocks and `mixed_capabilities()` to inspect installed optional engines.
