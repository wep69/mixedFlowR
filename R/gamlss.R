#' Fit a mixed GAMLSS model
#' @param formula Location (`mu`) formula.
#' @param data Data frame.
#' @param family GAMLSS family.
#' @param sigma.formula,nu.formula,tau.formula Distributional parameter formulas.
#' @param random Optional grouping-factor name added through `gamlss::random`.
#' @param re_random Optional `nlme`-style random formula added through `gamlss::re`.
#' @param correlation Optional `nlme` correlation structure used with `re_random`.
#' @param ... Additional `gamlss::gamlss` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: Gaussian GAMLSS with subject random effect
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) mixed_gamlss(height ~ treatment * 
#'       time, d, family = "NO", random = "subject")
#' }
#' # Example 2: model scale by treatment
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) mixed_gamlss(height ~ time, 
#'       d, family = "NO", sigma.formula = ~treatment, random = "subject")
#' }
#' # Example 3: random effect represented through nlme
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE) && requireNamespace("nlme", 
#'       quietly = TRUE)) mixed_gamlss(height ~ time, d, family = "NO", re_random = ~1 | 
#'       subject)
#' }
mixed_gamlss <- function(formula,data,family="NO",sigma.formula=~1,nu.formula=~1,tau.formula=~1,random=NULL,re_random=NULL,correlation=NULL,...) {
  .mf_need("gamlss","mixed GAMLSS models"); f<-formula
  if(!is.null(random)) {
    f<-stats::as.formula(paste(.mf_formula_text(f),"+ gamlss::random(",.mf_bt(random),")"))
  }
  # gamlss resolves its smoother terms, including re(), through the search path and
  # re-reads the data argument of its own call from the calling frame. With only the
  # namespace loaded and the data hidden in a formula environment, re() looks up
  # 'data' and finds utils::data. Attaching gamlss and keeping the data plus the
  # random specification as objects of THIS frame is what makes the documented
  # random-effect interface usable from inside another package.
  ..mf_re_random<-re_random; ..mf_re_cor<-correlation; ..mf_data<-data
  if(!is.null(re_random)) {
    f<-stats::update.formula(f,.~.+re(random=..mf_re_random,correlation=..mf_re_cor))
  }
  environment(f)<-environment()
  attached<-character(0)
  for(p in c("gamlss","gamlss.dist")) {
    if(requireNamespace(p,quietly=TRUE) && !paste0("package:",p) %in% search()) {
      suppressPackageStartupMessages(attachNamespace(asNamespace(p))); attached<-c(attached,p)
    }
  }
  on.exit(for(p in rev(attached)) try(detach(paste0("package:",p),character.only=TRUE,unload=FALSE),silent=TRUE),add=TRUE)
  # gamlss::re() resolves the data argument of the gamlss call in the global
  # environment, so a name that lives in this frame is invisible to it. Only for
  # that route is the data embedded in the call itself; the ordinary routes keep
  # the small, re-evaluable call.
  data_arg<-if(is.null(re_random)) quote(..mf_data) else data
  cl<-as.call(c(list(quote(gamlss::gamlss),formula=f,data=data_arg,
                     sigma.formula=sigma.formula,nu.formula=nu.formula,tau.formula=tau.formula,
                     family=family),list(...)))
  fit<-eval(cl)
  .mf_wrap(fit,"gamlss",match.call(),data,specification=list(family=family,random=random,re_random=re_random))
}

#' Extract fitted GAMLSS distribution parameters
#' @param object Fitted GAMLSS object.
#' @param parameters Parameters to attempt: `mu`, `sigma`, `nu`, `tau`.
#' @return A data frame of fitted distributional parameters.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: mu and sigma
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_parameters(m)
#'   }
#' }
#' # Example 2: mu only
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_parameters(m, "mu")
#'   }
#' }
#' # Example 3: heteroscedastic scale model
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, sigma.formula = ~treatment, random = "subject")
#'       mixed_gamlss_parameters(m)
#'   }
#' }
mixed_gamlss_parameters <- function(object,parameters=c("mu","sigma","nu","tau")) {
  m<-.mf_model(object); if(.mf_engine(object)!="gamlss") stop("A GAMLSS fit is required.",call.=FALSE)
  mu<-tryCatch(stats::fitted(m,what="mu"),error=function(e) stats::fitted(m))
  out<-data.frame(row=seq_along(mu))
  for(p in parameters) {z<-tryCatch(stats::fitted(m,what=p),error=function(e) NULL); if(!is.null(z)) out[[p]]<-as.numeric(z)}
  out
}

#' Compare GAMLSS candidates with generalized AIC
#' @param ... GAMLSS fits.
#' @param k Penalty per effective degree of freedom; `2` gives AIC and `log(n)` gives a BIC-like penalty.
#' @return Candidate score table. No model is selected automatically.
#' @export
#' @examples
#' # Example 1: no candidates
#' mixed_gamlss_select()
#' # Example 2: compare scale structures
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       a <- mixed_gamlss(height ~ time, d, random = "subject")
#'       b <- mixed_gamlss(height ~ time, d, sigma.formula = ~treatment, random = "subject")
#'       mixed_gamlss_select(a, b)
#'   }
#' }
#' # Example 3: stronger complexity penalty
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       a <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_select(a, k = log(nrow(d)))
#'   }
#' }
mixed_gamlss_select <- function(...,k=2) {
  xs<-list(...); if(!length(xs)) return(data.frame()); .mf_need("gamlss","GAMLSS comparison")
  nm<-names(xs); if(is.null(nm)) nm<-paste0("model",seq_along(xs)); nm[nm==""]<-paste0("model",which(nm==""))
  data.frame(model=nm,GAIC=vapply(xs,function(x) as.numeric(gamlss::GAIC(.mf_model(x),k=k)),numeric(1)),k=k)
}

#' Obtain fitted conditional quantiles from a GAMLSS model
#' @param object Fitted GAMLSS object.
#' @param probs Probabilities.
#' @return Long data frame with fitted quantiles for observed rows.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: quartiles
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE) && requireNamespace("gamlss.dist", 
#'       quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_quantiles(m, c(0.25, 0.5, 0.75))
#'   }
#' }
#' # Example 2: median
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE) && requireNamespace("gamlss.dist", 
#'       quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_quantiles(m, 0.5)
#'   }
#' }
#' # Example 3: central 95 percent limits
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE) && requireNamespace("gamlss.dist", 
#'       quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, sigma.formula = ~treatment, random = "subject")
#'       mixed_gamlss_quantiles(m, c(0.025, 0.975))
#'   }
#' }
mixed_gamlss_quantiles <- function(object,probs=c(.025,.5,.975)) {
  .mf_need("gamlss.dist","GAMLSS fitted quantiles"); m<-.mf_model(object); pars<-mixed_gamlss_parameters(object); fam<-as.character(m$family[1]); qname<-paste0("q",fam)
  if(!exists(qname,envir=asNamespace("gamlss.dist"),inherits=FALSE)) stop("No quantile function was found for family '",fam,"'.",call.=FALSE); qfun<-get(qname,envir=asNamespace("gamlss.dist"))
  ans<-lapply(probs,function(pr){args<-as.list(pars[setdiff(names(pars),"row")]); args$q<-NULL; args$p<-pr; q<-do.call(qfun,args); data.frame(row=pars$row,prob=pr,quantile=as.numeric(q))}); do.call(rbind,ans)
}

#' Diagnose a GAMLSS fit
#' @param object Fitted GAMLSS model.
#' @return Residual summary and publication-ready residual plot.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: default diagnostic
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, random = "subject")
#'       mixed_gamlss_diagnose(m)
#'   }
#' }
#' # Example 2: scale model
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ time, d, sigma.formula = ~treatment, random = "subject")
#'       mixed_gamlss_diagnose(m)
#'   }
#' }
#' # Example 3: inspect residual table
#' \donttest{
#'   if (requireNamespace("gamlss", quietly = TRUE)) {
#'       m <- mixed_gamlss(height ~ treatment * time, d, random = "subject")
#'       mixed_gamlss_diagnose(m)$data
#'   }
#' }
mixed_gamlss_diagnose <- function(object) {
  m<-.mf_model(object); r<-as.numeric(stats::residuals(m)); f<-as.numeric(stats::fitted(m)); d<-data.frame(fitted=f,residual=r)
  p<-ggplot2::ggplot(d,ggplot2::aes(x=.data$fitted,y=.data$residual))+ggplot2::geom_point(alpha=.65)+ggplot2::geom_hline(yintercept=0,lty=2)+.mf_theme()+ggplot2::labs(x="Fitted location",y="Normalized quantile residual")
  list(data=d,summary=summary(r),plot=p)
}
