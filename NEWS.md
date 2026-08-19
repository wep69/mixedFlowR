# mixedFlowR 0.1.0.9000

* Initial development implementation of blocks 1-70.

## Vignettes

* The vignette section is now organised as a Start Here overview followed by five
  progressive instructional blocks (foundations and core models; dependence,
  quantitative effects, diagnostics and robustness; advanced inference and
  distributional modelling; spatial, MET, publication and teaching; state of the
  art, validation and complete workflows). `_pkgdown.yml` publishes that order.
* The twenty focused tutorials were replaced by full teaching versions (14 to 41
  code chunks each, learning objectives, interpretation notes, exercises and
  references) and `v00-start-here-overview.Rmd` was added as the orientation map
  derived from the foundations-to-advanced tutorial.
* Vignette file names now begin with a letter (`v01-...` instead of `01-...`),
  which is required for the files installed under `inst/doc`.
* Vignette code is evaluated. The previous snapshot set `eval = FALSE` in every
  vignette, so "re-building of vignette outputs ... OK" proved only that the
  documents converted, never that the analyses ran. Optional engines are declared
  per chunk with `requireNamespace()`; the only non-evaluated blocks are Stan
  sampling, file-writing demonstrations and the release commands that cannot run
  inside the check they audit, and each says so in the text.

## Fixes during local validation (Windows, R 4.6.0)

* `R/block-registry.R` did not parse. The 70-block registry was written as
  `list(1 = list(...), 2 = list(...), ...)`, and a bare number is not a valid
  argument tag in R, so the file failed at `1 =` (line 3, column 5). Because
  nothing in the package could be parsed, `pkgload::load_all()`,
  `roxygen2::roxygenise()`, `R CMD build` and `R CMD check` were all blocked at
  the entry point. Root cause: the registry was authored in an environment
  without an R runtime and the static gate only balanced delimiters. Fix: quote
  the 70 tags (`"1" = list(...)`), which keeps the block numbers visible in the
  source while `mixed_blocks()` continues to read the registry by position
  (`seq_along()` plus `[[i]]`), so no behaviour changed.

* Fits are now recorded with a re-evaluable call. Estimation used to run as
  `lme4::lmer(formula, data = data, ...)` inside the wrapper, so the stored call
  referred to the internal variable names. Every backend that rebuilds the model
  from its call therefore failed: `lmerTest::as_lmerModLmerTest()` could not
  extract the deviance function, which removed both Satterthwaite and
  Kenward-Roger inference; `pbkrtest::PBmodcomp()` resolved `data` to
  `utils::data` and refused the parametric-bootstrap comparison; `emmeans` could
  not recover the data for models whose formula environment was a namespace. Fix:
  the data is bound under a stable name in an environment that travels with the
  formula (`.mf_fit()`), so the recorded call stays small and re-evaluable. All
  `lme4`, `nlme`, `glmmTMB` and `robustlmm` entry points use it.

* `mixed_anova()` called `lmerTest::anova()`, which `lmerTest` does not export;
  the S3 method is reached through `stats::anova()` on the converted fit.

* `mixed_anova(ddf = "Kenward-Roger")` now refuses an ML fit with the scientific
  reason (the correction uses the REML covariance of the variance components)
  instead of forwarding a backend message about deviance functions.

* `mixed_influence()` works again. `influence.ME` does not evaluate the model
  call: it takes the third element as text and looks the data up with `get()`,
  and it also expects `lme4` to be attached. The adapter renames the data
  argument to the name `influence.ME` reserves for model-frame data and attaches
  `lme4` for the duration of the call.

* `mixed_gxe()` was calling the previous reduced-rank interface. In the current
  stack `rrm()` is supplied by `enhancer`, takes a two-way table of identifiers
  by features, and must be wrapped in a structure function. The factor-analytic
  model now builds the genotype-by-environment table of observed means, refuses
  an incomplete grid or an impossible number of factors with a design-level
  message, and fits `vsm(usm(rrm(...)), ism(...))`. `sommer` and `enhancer` are
  attached for the call because `sommer` resolves its structure functions through
  the search path.

* `mixed_gamlss()` supports both random-effect routes again. `gamlss` resolves
  `re()` through the search path and reads the data argument of its own call in
  the global environment, so the package attaches `gamlss` for the fit and, on
  that route only, passes the data in the call itself.

* `mixed_spatial_field()` accepts design terms (`fixed`, `random`) as one-sided
  formulas or as column names, the string-first idiom used elsewhere in the
  package; an absent column is reported as a design error before estimation.
  Previously `"block"` reached `SpATS` as an invalid formula.

* `mixed_design_audit()` and `mixed_pseudorep()` work for repeated-measure
  designs with no treatment factor declared; the cluster screen used a
  length-one placeholder that made `table()` fail.

* Bootstrap intervals carry the name of the parameter they belong to, and
  `mixed_table(component = "bootstrap")` uses it. An unnamed interval matrix used
  to make the table fail outright.

## Documentation

* `man/` is generated by roxygen2 from the authoritative source comments. The
  snapshot shipped hand-written Rd files that roxygen skipped, so documentation
  and code could drift apart. Regeneration also removed every `\usage` line wider
  than 90 columns, and the long `@examples` lines were reflowed by re-parsing and
  deparsing each expression, so no example was broken by a blind text wrap.
* `\dontrun` is now used only where an example would sample with Stan or run a
  production-size bootstrap; everything else is `\donttest` and therefore runs
  under `R CMD check --as-cran`.

## Validation tooling

* `tools/static_audit.py` now parses the R sources through the R runtime when
  `Rscript` is available, instead of relying only on the lexical delimiter
  balance that let an unparseable `R/` tree pass. A check that cannot be
  performed is reported as `not performed` and no longer counts as a pass.
  The report no longer claims that R is unavailable when it is.
* `tests/testthat/test-registry-data.R` gained regression guards for the block
  registry: positional tags `"1"`..`"70"` in order, every registered function
  exported and existing, and shipped block metadata identical to the internal
  registry.
* `tests/testthat/test-backend-adapters.R` records one guard per defect above,
  each with the reason it matters: small-sample tests after a wrapped fit, the
  REML requirement of Kenward-Roger, parametric-bootstrap refitting, influence
  through `influence.ME`, audits of repeated-measure designs, named bootstrap
  intervals, the GxE covariance models with their design refusals, both GAMLSS
  random-effect routes, and spatial design terms.
