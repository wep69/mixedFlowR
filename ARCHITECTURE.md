# mixedFlowR Architecture

## Mission

`mixedFlowR` provides a design-aware and auditable workflow for mixed-model analyses in agronomy and related experimental sciences. It preserves experimental units, randomization, covariance assumptions, estimands and uncertainty while delegating estimation to established R engines.

## Scientific layers

1. **Design audit and legacy experimental analysis**: blocks 1-10.
2. **Core LMM and GLMM**: blocks 11-21.
3. **Residual and structured covariance**: blocks 22-31.
4. **Robust estimation and sensitivity**: blocks 32-36.
5. **Bootstrap uncertainty**: blocks 37-40.
6. **Mixed GAMLSS/distributional modelling**: blocks 41-45.
7. **Bayesian multilevel modelling**: blocks 46-51.
8. **Qualitative and quantitative inference**: blocks 52-56.
9. **Nonlinear and random-curve models**: blocks 57-60.
10. **Spatial field trials and multi-environment GxE**: blocks 61-64.
11. **Diagnostics and comparison**: blocks 65-67.
12. **Tables, graphics and teaching**: blocks 68-70.

## Object contract

Most model-fitting functions return `mixedflow_fit`:

```text
mixedflow_fit
├── call
├── engine
├── model              native backend object
├── data               stored analysis data
├── design             optional design metadata
├── specification      family/covariance/random structure
├── warnings
└── audit              timestamp and backend provenance
```

The native model is always retained. The wrapper does not alter coefficients or silently translate an incompatible model to a simpler backend.

## Backend policy

Core dependencies are deliberately small. Heavy or specialized engines remain in `Suggests` and are checked at call time. The intended routing is:

- `lme4`: LMM/GLMM, random intercepts/slopes, nesting/crossing and modern structured random-effect covariance terms.
- `nlme`: Gaussian mixed models requiring explicit residual heteroscedasticity or residual correlation, including continuous-time correlation.
- `glmmTMB`: flexible GLMMs, zero inflation, dispersion modelling and structured covariance.
- `robustlmm`: robust linear mixed models.
- `confintROB`: robust/classical bootstrap confidence intervals.
- `gamlss`: distributional regression with mixed/random-effect terms.
- `brms`: Bayesian multilevel, nonlinear and distributional models.
- `SpATS`: two-dimensional P-spline field-trial correction.
- `sommer`: complex covariance, multi-environment and genomic mixed models.

## Inference policy

The package does not select a model solely by p-value, AIC or BIC. It first checks scientific/design admissibility, then computational status and diagnostics, and only then reports comparison metrics. Quantitative factors remain quantitative unless the analyst explicitly recodes them.

## Documentation policy

Roxygen comments are authoritative. `NAMESPACE` and `man/` are release artifacts and must be regenerated with `roxygen2::roxygenise()` before release. Every public function contains three examples, and the API example vignette mirrors these examples.
