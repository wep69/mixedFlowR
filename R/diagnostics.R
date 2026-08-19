#' Diagnose a mixed-model fit
#' @param object Fitted model.
#' @param simulate If TRUE and DHARMa supports the model, compute simulation-based residual diagnostics.
#' @param nsim Number of DHARMa simulations.
#' @return A `mixedflow_diagnostics` list with convergence, singularity, residual data and optional simulation diagnostics.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: LMM diagnostics
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_diagnose(m)
#'   }
#' }
#' # Example 2: random-slope singularity screen
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_random_slopes(d, "height", "time", "time", "subject")
#'       mixed_diagnose(m)
#'   }
#' }
#' # Example 3: simulation diagnostics for a count model
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE) && requireNamespace("DHARMa", 
#'       quietly = TRUE)) {
#'       c <- mixed_data("counts")
#'       m <- mixed_count(count ~ treatment * time + (1 | plot), c, "nbinom2")
#'       mixed_diagnose(m, simulate = TRUE, nsim = 50)
#'   }
#' }
mixed_diagnose <- function(object,simulate=FALSE,nsim=250) {
  m<-.mf_model(object); eng<-.mf_engine(object); conv<-TRUE; msg<-character(); singular<-NA
  if(eng=="lme4") {singular<-lme4::isSingular(m); oi<-m@optinfo; if(length(oi$conv$lme4$messages)) {conv<-FALSE;msg<-oi$conv$lme4$messages}}
  if(eng=="glmmTMB") {conv<-isTRUE(m$sdr$pdHess); if(!conv) msg<-"Non-positive-definite Hessian or convergence diagnostic."}
  r<-tryCatch(as.numeric(stats::residuals(m)),error=function(e) numeric()); f<-tryCatch(as.numeric(stats::fitted(m)),error=function(e) numeric()); rd<-if(length(r)&&length(f)==length(r)) data.frame(fitted=f,residual=r) else data.frame()
  sim<-NULL; if(simulate) { .mf_need("DHARMa","simulation-based residual diagnostics"); sim<-DHARMa::simulateResiduals(fittedModel=m,n=nsim,plot=FALSE) }
  structure(list(engine=eng,converged=conv,convergence_messages=msg,singular=singular,residuals=rd,simulation=sim,variance=.mf_tidy_varcorr(object)),class="mixedflow_diagnostics")
}

#' Influence diagnostics for grouped mixed models
#' @param object Fitted mixed model.
#' @param group Grouping factor for deletion diagnostics.
#' @param ... Additional `influence.ME::influence` arguments.
#' @return Native influence object when supported, otherwise a residual sensitivity screen.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: subject deletion diagnostics
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("influence.ME", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_influence(m, "subject")
#'   }
#' }
#' # Example 2: treatment model
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("influence.ME", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_influence(m, "subject")
#'   }
#' }
#' # Example 3: robust residual fallback
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) {
#'       m <- mixed_robust(height ~ time + (1 | subject), d)
#'       mixed_influence(m, "subject")
#'   }
#' }
mixed_influence <- function(object,group,...) {
  eng<-.mf_engine(object); if(eng=="lme4") { .mf_need("influence.ME","mixed-model influence diagnostics"); m<-.mf_influence_ready(.mf_model(object)); return(.mf_with_attached("lme4", influence.ME::influence(m,group=group,...))) }
  mixed_robust_influence(object,cluster=group)
}

.mf_fixed_signature <- function(x) {
  m<-.mf_model(x); eng<-.mf_engine(x)
  f<-tryCatch(stats::formula(m),error=function(e) NULL); if(is.null(f)) return(NA_character_)
  if(eng=="lme4" && requireNamespace("reformulas",quietly=TRUE)) f<-reformulas::nobars(f)
  .mf_formula_text(f)
}

#' Compare mixed models with design-aware safeguards
#' @param ... Fitted models.
#' @param method `information`, `LRT`, `Kenward-Roger`, `parametric-bootstrap`, or `auto`.
#' @param nsim Number of simulations for parametric-bootstrap comparison.
#' @param seed Reproducible seed for parametric-bootstrap comparison.
#' @return A comparison table or likelihood-ratio ANOVA.
#' @details The function blocks likelihood-ratio tests of different fixed-effect structures when an `lmer` model is still fitted by REML. Information criteria are reported as descriptors, not as an automatic scientific decision rule.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: information criteria
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       a <- mixed_fit(height ~ time + (1 | subject), d, REML = FALSE)
#'       b <- mixed_fit(height ~ treatment * time + (1 | subject), d, REML = FALSE)
#'       mixed_compare(a, b)
#'   }
#' }
#' # Example 2: valid fixed-effect LRT under ML
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       a <- mixed_fit(height ~ time + (1 | subject), d, REML = FALSE)
#'       b <- mixed_fit(height ~ treatment + time + (1 | subject), d, REML = FALSE)
#'       mixed_compare(a, b, method = "LRT")
#'   }
#' }
#' # Example 3: covariance candidates
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) {
#'       a <- mixed_ar1(d, "height", "time", "time", "subject")
#'       b <- mixed_car1(d, "height", "time", "time", "subject")
#'       mixed_compare(a, b)
#'   }
#' }
mixed_compare <- function(...,method=c("auto","information","LRT","Kenward-Roger","parametric-bootstrap"),nsim=999,seed=123) {
  xs<-list(...); method<-match.arg(method); if(length(xs)<2L) stop("Supply at least two fitted models.",call.=FALSE)
  if(method=="auto") method<-"information"
  if(method %in% c("Kenward-Roger","parametric-bootstrap")) {
    .mf_need("pbkrtest","small-sample mixed-model comparison"); ms<-lapply(xs,.mf_model); if(length(ms)!=2L||!all(vapply(ms,inherits,logical(1),what="lmerMod"))) stop("Kenward-Roger and parametric-bootstrap comparisons require exactly two lmer models.",call.=FALSE)
    nfix<-vapply(ms,function(m) length(lme4::fixef(m)),integer(1))
    large<-ms[[which.max(nfix)]]; small<-ms[[which.min(nfix)]]
    if(nfix[1]==nfix[2]) {large<-ms[[2]];small<-ms[[1]]}
    if(method=="Kenward-Roger") return(pbkrtest::KRmodcomp(large,small))
    return(pbkrtest::PBmodcomp(large,small,nsim=nsim,seed=seed))
  }
  if(method=="LRT") {
    sig<-vapply(xs,.mf_fixed_signature,character(1)); different<-length(unique(sig[!is.na(sig)]))>1L
    if(different) for(x in xs) if(.mf_engine(x)=="lme4" && inherits(.mf_model(x),"lmerMod") && lme4::isREML(.mf_model(x))) stop("LRTs for models with different fixed effects require ML rather than REML. Refit with REML = FALSE.",call.=FALSE)
    return(do.call(stats::anova,lapply(xs,.mf_model)))
  }
  nm<-names(xs); if(is.null(nm)) nm<-rep("",length(xs)); nm[nm==""]<-paste0("model",which(nm==""))
  do.call(rbind,lapply(seq_along(xs),function(i){m<-.mf_model(xs[[i]]); data.frame(model=nm[i],engine=.mf_engine(xs[[i]]),fixed=.mf_fixed_signature(xs[[i]]),npar=tryCatch(attr(stats::logLik(m),"df"),error=function(e) NA_integer_),logLik=tryCatch(as.numeric(stats::logLik(m)),error=function(e) NA_real_),AIC=tryCatch(stats::AIC(m),error=function(e) NA_real_),BIC=tryCatch(stats::BIC(m),error=function(e) NA_real_))}))
}
