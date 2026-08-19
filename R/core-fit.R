#' Fit a mixed model through an appropriate engine
#'
#' Provides a common entry point for `lme4`, `nlme` and `glmmTMB` while retaining the original backend object.
#' Automatic routing is conservative: Gaussian models with residual correlation/variance structures go to `nlme`;
#' otherwise Gaussian and standard GLMM formulas go to `lme4` unless an engine is requested explicitly.
#'
#' @param formula Model formula. For `lme4`/`glmmTMB`, random effects may use `|` terms.
#' @param data Data frame.
#' @param family A base-R family object or family name.
#' @param engine One of `auto`, `lme4`, `nlme`, or `glmmTMB`.
#' @param random `nlme` random-effects formula/list when applicable.
#' @param correlation Optional `nlme` correlation structure.
#' @param weights Optional `nlme` variance function/weights.
#' @param REML Logical; use REML for Gaussian `lme4`/`nlme` fits.
#' @param ziformula,dispformula `glmmTMB` zero-inflation and dispersion formulas.
#' @param ... Additional backend arguments.
#' @return A `mixedflow_fit` wrapper retaining the native fitted model.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: Gaussian split-plot LMM
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_fit(yield ~ variety * nitrogen + 
#'       (1 | block) + (1 | block:variety), d)
#' }
#' # Example 2: longitudinal LMM with random slope
#' z <- mixed_data("longitudinal")
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_fit(height ~ treatment * 
#'       time + (time | subject), z)
#' }
#' # Example 3: correlated residuals via nlme
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE))
#'     mixed_fit(height ~ treatment * time, z, engine = "nlme",
#'               random = ~time | subject,
#'               correlation = nlme::corAR1(form = ~time | subject))
#' }
mixed_fit <- function(formula, data, family=stats::gaussian(), engine=c("auto","lme4","nlme","glmmTMB"), random=NULL,
                      correlation=NULL, weights=NULL, REML=TRUE, ziformula=~0, dispformula=~1, ...) {
  engine <- match.arg(engine); family <- .mf_family(family)
  if (engine=="auto") {
    if (!is.null(correlation) || !is.null(weights) || !is.null(random)) engine <- "nlme"
    else engine <- "lme4"
  }
  call <- match.call()
  if (engine=="lme4") {
    .mf_need("lme4","mixed-model fitting")
    if (inherits(family,"family") && identical(family$family,"gaussian")) {
      m <- .mf_fit(quote(lme4::lmer),formula,data,REML=REML,...)
    } else {
      m <- .mf_fit(quote(lme4::glmer),formula,data,family=family,...)
    }
  } else if (engine=="nlme") {
    .mf_need("nlme","residual covariance/heterogeneity models")
    if (!inherits(family,"family") || !identical(family$family,"gaussian")) stop("nlme::lme is Gaussian; use glmmTMB/brms/GAMLSS for non-Gaussian responses.",call.=FALSE)
    if (is.null(random)) stop("For engine='nlme', supply the random argument explicitly.",call.=FALSE)
    m <- .mf_fit(quote(nlme::lme),formula,data,formula_arg="fixed",random=random,
                 correlation=correlation, weights=weights,
                 method=if(REML) "REML" else "ML", ...)
  } else {
    .mf_need("glmmTMB","flexible generalized mixed models")
    m <- .mf_fit(quote(glmmTMB::glmmTMB),formula,data,family=family,ziformula=ziformula,dispformula=dispformula,REML=REML,...)
  }
  .mf_wrap(m,engine,call=call,data=data,specification=list(formula=formula,family=family,REML=REML))
}

#' Fit a random-intercept mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect right-hand-side expression as text.
#' @param group Grouping column.
#' @param engine Backend passed to [mixed_fit()].
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' z <- mixed_data("longitudinal")
#' # Example 1: treatment-time random intercept
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_intercepts(z, "height", 
#'       "treatment*time", "subject")
#' }
#' # Example 2: field block random intercept
#' d <- mixed_data("splitplot")
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_intercepts(d, "yield", 
#'       "variety*nitrogen", "block")
#' }
#' # Example 3: count GLMM random intercept
#' c <- mixed_data("counts")
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_intercepts(c, "count", 
#'       "treatment*time", "plot", family = poisson())
#' }
mixed_random_intercepts <- function(data,response,fixed,group,engine="lme4",...) {
  f <- stats::as.formula(paste(.mf_bt(response),"~",fixed,"+ (1|",.mf_bt(group),")")); mixed_fit(f,data,engine=engine,...)
}

#' Fit a random-slope mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param slope Quantitative random-slope variable.
#' @param group Grouping variable.
#' @param correlated If `FALSE`, use separate intercept and slope variance terms.
#' @param ... Additional arguments to [mixed_fit()].
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' z<-mixed_data("longitudinal")
#' # Example 1: correlated intercept/slope
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_slopes(z, "height", 
#'       "treatment*time", "time", "subject")
#' }
#' # Example 2: diagonal intercept/slope covariance
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_slopes(z, "height", 
#'       "treatment*time", "time", "subject", correlated = FALSE)
#' }
#' # Example 3: centered time
#' z$tc <- z$time - mean(z$time)
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_slopes(z, "height", 
#'       "treatment*tc", "tc", "subject")
#' }
mixed_random_slopes <- function(data,response,fixed,slope,group,correlated=TRUE,...) {
  rt <- if(correlated) paste0("(1 + ",.mf_bt(slope)," | ",.mf_bt(group),")") else paste0("(1 | ",.mf_bt(group),") + (0 + ",.mf_bt(slope)," | ",.mf_bt(group),")")
  f<-stats::as.formula(paste(.mf_bt(response),"~",fixed,"+",rt)); mixed_fit(f,data,...)
}

#' Fit nested random effects
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param groups Character vector ordered from outer to inner grouping factor.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("splitplot"); d$wp<-interaction(d$block,d$variety)
#' # Example 1: block/whole-plot nesting
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_nested(d, "yield", "variety*nitrogen", 
#'       c("block", "wp"))
#' }
#' # Example 2: one nesting level
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_nested(d, "yield", "variety*nitrogen", 
#'       "block")
#' }
#' # Example 3: factor-coded nitrogen
#' d$nf <- factor(d$nitrogen)
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_nested(d, "yield", "variety*nf", 
#'       c("block", "wp"))
#' }
mixed_nested <- function(data,response,fixed,groups,...) {
  if(!length(groups)) stop("groups must contain at least one grouping factor.",call.=FALSE)
  term<-paste0("(1|",paste(vapply(groups,.mf_bt,character(1)),collapse="/"),")")
  f<-stats::as.formula(paste(.mf_bt(response),"~",fixed,"+",term)); mixed_fit(f,data,...)
}

#' Fit crossed random effects
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param groups Character vector of crossed grouping factors.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("met"); d$year<-factor(rep(1:3,length.out=nrow(d)))
#' # Example 1: genotype and environment random effects
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_crossed(d, "yield", "1", 
#'       c("genotype", "environment"))
#' }
#' # Example 2: genotype and replicate
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_crossed(d, "yield", "environment", 
#'       c("genotype", "rep"))
#' }
#' # Example 3: three crossed sources
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_crossed(d, "yield", "1", 
#'       c("genotype", "environment", "year"))
#' }
mixed_crossed <- function(data,response,fixed,groups,...) {
  terms<-paste0("(1|",vapply(groups,.mf_bt,character(1)),")",collapse=" + ")
  f<-stats::as.formula(paste(.mf_bt(response),"~",fixed,"+",terms)); mixed_fit(f,data,...)
}

#' Extract conditional random-effect predictions (BLUPs/modes)
#' @param object Fitted object or `mixedflow_fit`.
#' @param group Optional grouping component.
#' @return Backend-specific random-effect table/list.
#' @export
#' @examples
#' z<-mixed_data("longitudinal")
#' # Example 1: random intercept and slope BLUPs
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       f <- mixed_fit(height ~ treatment * time + (time | subject), z)
#'       mixed_blup(f)
#'   }
#' }
#' # Example 2: selected grouping factor
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       f <- mixed_fit(height ~ time + (1 | subject), z)
#'       mixed_blup(f, "subject")
#'   }
#' }
#' # Example 3: glmmTMB conditional effects
#' c <- mixed_data("counts")
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) {
#'       f <- mixed_glmm(count ~ treatment + (1 | plot), c, family = poisson(), 
#'           engine = "glmmTMB")
#'       mixed_blup(f)
#'   }
#' }
mixed_blup <- function(object,group=NULL) {
  eng<-.mf_engine(object); m<-.mf_model(object)
  if(eng%in%c("lme4","robustlmm")){x<-lme4::ranef(m,condVar=TRUE); if(is.null(group)) x else x[[group]]}
  else if(eng=="nlme"){x<-nlme::ranef(m); x}
  else if(eng=="glmmTMB"){x<-glmmTMB::ranef(m)$cond; if(is.null(group)) x else x[[group]]}
  else if(eng=="brms"){x<-brms::ranef(m,summary=TRUE); if(is.null(group)) x else x[[group]]}
  else stop("BLUP/random-effect extraction is unavailable for this engine.",call.=FALSE)
}
