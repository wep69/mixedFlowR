# mixedFlowR Implementation Plan and Block Map

The 70 requested blocks are implemented as the primary public workflow. `inst/metadata/block_registry.csv` is the machine-readable source of the mapping.

| Blocks | Layer | Principal capability |
|---:|---|---|
| 1-4 | Design audit | hierarchy, balance, experimental units, pseudoreplication screening |
| 5-10 | Legacy experimental | split-plot, EMS, ANOVA strata, split-split, strip-plot, legacy-to-mixed comparison |
| 11-16 | LMM core | common fit, intercepts, slopes, nesting, crossing, BLUPs |
| 17-21 | GLMM | generic GLMM, binomial, counts, beta, zero inflation |
| 22-31 | Covariance | heterogeneity, AR1/CAR1, Toeplitz, spatiotemporal and spatial covariance |
| 32-36 | Robust | robust fitting, influence, sensitivity, contamination screen, robust covariance |
| 37-40 | Bootstrap | generic, fixed-effect, variance-component and predictive bootstrap |
| 41-45 | GAMLSS | mixed GAMLSS, fitted parameters, candidate comparison, quantiles, diagnostics |
| 46-51 | Bayesian | fitting, priors, prior predictive, posterior, PPC, LOO |
| 52-56 | Inference | EMMs, contrasts, CLD, trends, curves |
| 57-60 | Nonlinear | generic nonlinear mixed, growth, dose-response, random curves |
| 61-64 | Agronomic advanced | SpATS field trials, surfaces, MET and GxE covariance |
| 65-67 | Diagnostics | diagnostics, influence and safeguarded model comparison |
| 68-70 | Communication/teaching | tables, publication figures and teaching tour |

Additional support APIs include `mixed_anova()`, `mixed_quantitative()`, `mixed_optimum()`, `mixed_report()`, `mixed_data()`, `mixed_capabilities()` and `mixed_blocks()`.
