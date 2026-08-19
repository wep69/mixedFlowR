#' Fit a generalized linear mixed model
#' @param formula Mixed-model formula.
#' @param data Data frame.
#' @param family Family specification.
#' @param engine `lme4` or `glmmTMB`.
#' @param ... Additional backend arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' c<-mixed_data("counts")
#' # Example 1: Poisson GLMM
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_glmm(count ~ treatment * 
#'       time + (1 | plot), c, poisson())
#' }
#' # Example 2: binomial incidence
#' b <- mixed_data("binomial")
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_glmm(cbind(diseased, healthy) ~ 
#'       treatment + (1 | block), b, binomial())
#' }
#' # Example 3: glmmTMB Poisson
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_glmm(count ~ treatment + 
#'       (1 | plot), c, poisson(), engine = "glmmTMB")
#' }
mixed_glmm <- function(formula,data,family,engine=c("lme4","glmmTMB"),...) {
  engine<-match.arg(engine); mixed_fit(formula,data,family=family,engine=engine,REML=FALSE,...)
}

#' Fit a binomial mixed model
#' @param data Data frame.
#' @param successes,failures Success and failure count columns.
#' @param fixed Fixed-effect RHS text.
#' @param group Grouping variable.
#' @param engine Backend.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' b<-mixed_data("binomial")
#' # Example 1: treatment incidence
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_binomial(b, "diseased", 
#'       "healthy", "treatment", "block")
#' }
#' # Example 2: include plot as grouping factor
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_binomial(b, "diseased", 
#'       "healthy", "treatment", "plot")
#' }
#' # Example 3: glmmTMB backend
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_binomial(b, "diseased", 
#'       "healthy", "treatment", "block", engine = "glmmTMB")
#' }
mixed_binomial <- function(data,successes,failures,fixed,group,engine="lme4",...) {
  lhs<-paste0("cbind(",.mf_bt(successes),",",.mf_bt(failures),")")
  f<-stats::as.formula(paste(lhs,"~",fixed,"+ (1|",.mf_bt(group),")")); mixed_glmm(f,data,stats::binomial(),engine=engine,...)
}

#' Fit a count mixed model
#' @param formula Mixed-model formula.
#' @param data Data frame.
#' @param family One of `poisson`, `nbinom1`, or `nbinom2`.
#' @param engine Backend; negative-binomial variants use `glmmTMB`.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' c<-mixed_data("counts")
#' # Example 1: Poisson
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_count(count ~ treatment * 
#'       time + (1 | plot), c, "poisson", "lme4")
#' }
#' # Example 2: negative-binomial 2
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_count(count ~ treatment * 
#'       time + (1 | plot), c, "nbinom2")
#' }
#' # Example 3: negative-binomial 1
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_count(count ~ treatment + 
#'       (1 | plot), c, "nbinom1")
#' }
mixed_count <- function(formula,data,family=c("nbinom2","nbinom1","poisson"),engine=NULL,...) {
  family<-match.arg(family)
  if(family=="poisson") {if(is.null(engine)) engine<-"lme4"; fam<-stats::poisson()}
  else {.mf_need("glmmTMB","negative-binomial GLMMs"); if(is.null(engine)) engine<-"glmmTMB"; fam<-if(family=="nbinom2") glmmTMB::nbinom2() else glmmTMB::nbinom1()}
  mixed_glmm(formula,data,fam,engine=engine,...)
}

#' Fit a beta mixed model
#' @param formula Mixed-model formula for a response strictly inside `(0,1)`.
#' @param data Data frame.
#' @param link Link function.
#' @param ... Additional `glmmTMB` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' b<-mixed_data("beta")
#' # Example 1: treatment beta mixed model
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_beta(severity ~ treatment + 
#'       (1 | block), b)
#' }
#' # Example 2: logit with plot grouping
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_beta(severity ~ treatment + 
#'       (1 | plot), b, "logit")
#' }
#' # Example 3: probit link
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_beta(severity ~ treatment + 
#'       (1 | block), b, "probit")
#' }
mixed_beta <- function(formula,data,link="logit",...) {
  .mf_need("glmmTMB","beta mixed models"); y<-stats::model.response(stats::model.frame(formula,data=data,na.action=stats::na.pass))
  if(any(y<=0|y>=1,na.rm=TRUE)) stop("Beta mixed models require responses strictly between 0 and 1; use an appropriate boundary model instead of silently squeezing data.",call.=FALSE)
  mixed_fit(formula,data,family=glmmTMB::beta_family(link=link),engine="glmmTMB",REML=FALSE,...)
}

#' Fit a zero-inflated or hurdle-style mixed model
#' @param formula Conditional mixed-model formula.
#' @param data Data frame.
#' @param family `glmmTMB` family object; defaults to negative-binomial 2.
#' @param ziformula Formula for structural-zero probability.
#' @param dispformula Formula for dispersion.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' c<-mixed_data("counts")
#' # Example 1: constant zero inflation
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_zero_inflated(count ~ 
#'       treatment * time + (1 | plot), c, ziformula = ~1)
#' }
#' # Example 2: treatment-dependent zero inflation
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_zero_inflated(count ~ 
#'       treatment + (1 | plot), c, ziformula = ~treatment)
#' }
#' # Example 3: dispersion model
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_zero_inflated(count ~ 
#'       treatment + (1 | plot), c, ziformula = ~1, dispformula = ~treatment)
#' }
mixed_zero_inflated <- function(formula,data,family=NULL,ziformula=~1,dispformula=~1,...) {
  .mf_need("glmmTMB","zero-inflated mixed models"); if(is.null(family)) family<-glmmTMB::nbinom2()
  mixed_fit(formula,data,family=family,engine="glmmTMB",ziformula=ziformula,dispformula=dispformula,REML=FALSE,...)
}
