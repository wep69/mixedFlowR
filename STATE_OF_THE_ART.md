# mixedFlowR State of the Art Snapshot

Review date: **2026-08-18**.

## Design rationale

The R mixed-model ecosystem is mature but methodologically distributed. `mixedFlowR` therefore acts as a design-aware orchestration layer rather than replacing established estimators. The package keeps classical experimental-error reasoning visible while exposing modern frequentist, robust, distributional, Bayesian, spatial and multi-environment engines through a coherent workflow.

## Current engine notes

- `lme4` 2.0-6 (2026-07-16) remains a central LMM/GLMM engine and the 2.x series adds tagged structured random-effect covariance terms. This does not make its structured random covariance identical to `nlme` residual correlation/heteroscedasticity.
- `glmmTMB` 1.1.14 (2026-01-15) supports flexible generalized mixed models, zero inflation, dispersion submodels and multiple covariance structures.
- `robustlmm` 3.5.0-2 (2026-07-30) supplies robust linear mixed-model estimation compatible with the modern `lme4` series.
- `confintROB` 1.1-1 (2026-01-29) provides Wald, parametric-bootstrap and wild-bootstrap intervals, including percentile/BCa routes where supported.
- `emmeans` 2.0.4 (2026-07-15) supplies marginal means, contrasts and trends. Compact-letter displays are treated only as a visualization of non-rejection patterns, never as proof of equality.
- `gamlss` 5.5-0 provides location-scale-shape distributional regression and random-effect terms.
- `brms` 2.23.0 provides Bayesian multilevel, nonlinear, autocorrelated and distributional modelling through Stan.
- `SpATS` 1.0-20 (2026-05-17) supplies two-dimensional P-spline spatial analysis for field trials.
- `sommer` 4.4.6 (2026-07-09) supplies complex covariance structures, multi-environment/genomic mixed models and reduced-rank GxE structures.
- `roxygen2` 8.1.0 (2026-08-04) is the documentation/NAMESPACE generator targeted by this source snapshot.

## Important methodological boundary

A common interface does not imply inferential equivalence. In particular:

1. an `nlme` residual AR(1) and an AR(1)-structured random component in another engine are not automatically the same statistical model;
2. a robust estimator changes the estimating equations and should be reported as sensitivity/robust inference, not as automatic outlier deletion;
3. GAMLSS and Bayesian distributional models may target multiple parameters of the conditional distribution, not only the mean;
4. GxE factor-analytic covariance and AMMI/GGE descriptive biplots answer related but non-identical questions;
5. information criteria are model descriptors conditional on the candidate set, not a replacement for design validity and diagnostics.

## Foundational references

The package bibliography and metadata audit include the foundational publications for `lme4`, `glmmTMB`, `robustlmm`, GAMLSS, `brms`, `SpATS` and `sommer`. See `vignettes/references.bib` and `inst/metadata/reference_verification.csv`.
