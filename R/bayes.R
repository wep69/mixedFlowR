#' Fit a Bayesian multilevel model
#' @param formula `brms` model formula.
#' @param data Data frame.
#' @param family Response family.
#' @param prior Optional `brms` prior specification.
#' @param chains Number of chains.
#' @param iter Iterations per chain.
#' @param backend Optional Stan backend.
#' @param seed Reproducible seed.
#' @param ... Additional `brms::brm` arguments.
#' @return A `mixedflow_fit` wrapping a `brmsfit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: Gaussian random intercept
#' \dontrun{mixed_bayes(height~treatment*time+(1|subject),d,chains=2,iter=1000)}
#' # Example 2: random slope
#' \dontrun{mixed_bayes(height~time+(time|subject),d,chains=2,iter=1000)}
#' # Example 3: binomial multilevel model
#' \dontrun{
#'   b <- mixed_data("binomial")
#'   mixed_bayes(diseased | trials(total) ~ treatment + (1 | block), b, family = binomial(), 
#'       chains = 2, iter = 1000)
#' }
mixed_bayes <- function(formula,data,family=stats::gaussian(),prior=NULL,chains=4,iter=2000,backend=NULL,seed=123,...) {
  .mf_need("brms","Bayesian mixed models"); args<-list(formula=formula,data=data,family=family,prior=prior,chains=chains,iter=iter,seed=seed); if(!is.null(backend)) args$backend<-backend; args<-c(args,list(...)); fit<-do.call(brms::brm,args)
  .mf_wrap(fit,"brms",match.call(),data,specification=list(family=family,chains=chains,iter=iter,seed=seed))
}

#' Inspect or design priors for a Bayesian mixed model
#' @param formula Model formula or a fitted `brmsfit`/`mixedflow_fit`.
#' @param data Data frame when a formula is supplied.
#' @param family Family when a formula is supplied.
#' @param ... Additional `brms::get_prior` arguments.
#' @return Prior table.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: available priors
#' \donttest{if(requireNamespace("brms",quietly=TRUE)) mixed_prior(height~time+(1|subject),d)}
#' # Example 2: random-slope priors
#' \donttest{if(requireNamespace("brms",quietly=TRUE)) mixed_prior(height~time+(time|subject),d)}
#' # Example 3: binomial prior classes
#' \donttest{
#'   if (requireNamespace("brms", quietly = TRUE)) {
#'       b <- mixed_data("binomial")
#'       mixed_prior(diseased | trials(total) ~ treatment + (1 | block), b, binomial())
#'   }
#' }
mixed_prior <- function(formula,data=NULL,family=stats::gaussian(),...) {
  .mf_need("brms","Bayesian prior specification"); if(inherits(formula,"mixedflow_fit")||inherits(formula,"brmsfit")) return(brms::prior_summary(.mf_model(formula)))
  brms::get_prior(formula=formula,data=data,family=family,...)
}

#' Run prior predictive simulation
#' @param formula Model formula or fitted Bayesian model.
#' @param data Data frame for a new model.
#' @param family Response family.
#' @param prior Prior specification.
#' @param chains,iter Stan sampling controls.
#' @param seed Seed.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit` whose posterior draws are generated from the prior only.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: prior-only Gaussian model
#' \dontrun{mixed_prior_predictive(height~time+(1|subject),d,chains=2,iter=500)}
#' # Example 2: treatment model
#' \dontrun{mixed_prior_predictive(height~treatment*time+(1|subject),d,chains=2,iter=500)}
#' # Example 3: update an existing brms fit
#' \dontrun{
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_prior_predictive(m, chains = 2, iter = 500)
#' }
mixed_prior_predictive <- function(formula,data=NULL,family=stats::gaussian(),prior=NULL,chains=4,iter=1000,seed=123,...) {
  .mf_need("brms","prior predictive simulation")
  if(inherits(formula,"mixedflow_fit")||inherits(formula,"brmsfit")) {
    old<-formula; fit<-stats::update(.mf_model(old),sample_prior="only",chains=chains,iter=iter,seed=seed,...); return(.mf_wrap(fit,"brms",match.call(),if(inherits(old,"mixedflow_fit")) old$data else data,specification=list(sample_prior="only")))
  }
  mixed_bayes(formula,data,family=family,prior=prior,chains=chains,iter=iter,seed=seed,sample_prior="only",...)
}

#' Extract posterior draws
#' @param object Bayesian fit.
#' @param variables Optional posterior variable selection passed to `posterior::subset_draws`.
#' @return A `draws_df` object.
#' @export
#' @examples
#' # Example 1: all draws
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_posterior(m)
#' }
#' # Example 2: fixed effect draws
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_posterior(m, "b_time")
#' }
#' # Example 3: multiple variables
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_posterior(m, c("b_Intercept", "b_time"))
#' }
mixed_posterior <- function(object,variables=NULL) {
  .mf_need("posterior","posterior draw extraction"); d<-posterior::as_draws_df(.mf_model(object)); if(!is.null(variables)) d<-posterior::subset_draws(d,variable=variables); d
}

#' Posterior predictive check
#' @param object Bayesian fit.
#' @param type `bayesplot`/`brms` predictive-check type.
#' @param ndraws Number of predictive draws displayed.
#' @param ... Additional `brms::pp_check` arguments.
#' @return A posterior predictive check plot.
#' @export
#' @examples
#' # Example 1: density overlay
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_pp_check(m)
#' }
#' # Example 2: ECDF overlay
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 500)
#'   mixed_pp_check(m, "ecdf_overlay")
#' }
#' # Example 3: interval PPC
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ treatment * time + (1 | subject), d, chains = 2, 
#'       iter = 500)
#'   mixed_pp_check(m, "intervals")
#' }
mixed_pp_check <- function(object,type="dens_overlay",ndraws=100,...) {
  .mf_need("brms","posterior predictive checks"); brms::pp_check(.mf_model(object),type=type,ndraws=ndraws,...)
}

#' Leave-one-out predictive assessment for Bayesian models
#' @param object One or more Bayesian fits.
#' @param ... Additional fits or arguments accepted by `brms::loo` when a single model is supplied.
#' @return A LOO object or a model-comparison table.
#' @export
#' @examples
#' # Example 1: single model
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 1000)
#'   mixed_loo(m)
#' }
#' # Example 2: treatment model
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   m <- mixed_bayes(height ~ treatment * time + (1 | subject), d, chains = 2, 
#'       iter = 1000)
#'   mixed_loo(m)
#' }
#' # Example 3: compare two models
#' \dontrun{
#'   d <- mixed_data("longitudinal")
#'   a <- mixed_bayes(height ~ time + (1 | subject), d, chains = 2, iter = 1000)
#'   b <- mixed_bayes(height ~ treatment * time + (1 | subject), d, chains = 2, 
#'       iter = 1000)
#'   mixed_loo(a, b)
#' }
mixed_loo <- function(object,...) {
  .mf_need("brms","Bayesian LOO assessment"); dots<-list(...)
  if(length(dots) && all(vapply(dots,function(x) inherits(x,"mixedflow_fit")||inherits(x,"brmsfit"),logical(1)))) {
    .mf_need("loo","LOO model comparison"); fits<-c(list(object),dots); los<-lapply(fits,function(x) brms::loo(.mf_model(x))); return(do.call(loo::loo_compare,los))
  }
  brms::loo(.mf_model(object),...)
}
