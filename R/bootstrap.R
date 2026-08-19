.mf_boot_fun <- function(statistic=c("fixed","variance")) {
  statistic<-match.arg(statistic)
  if(statistic=="fixed") return(function(x) unname(lme4::fixef(x)))
  function(x) {v<-as.data.frame(lme4::VarCorr(x)); c(stats::setNames(v$sdcor[v$var2==""|is.na(v$var2)],paste0(v$grp[v$var2==""|is.na(v$var2)],"_sd")),residual_sd=stats::sigma(x))}
}

#' Bootstrap inference for mixed models
#' @param object Fitted `lmer`/`rlmer` or wrapped fit.
#' @param statistic `fixed` or `variance`.
#' @param B Number of bootstrap replicates.
#' @param method `parametric` or `wild`.
#' @param interval `percentile` or `BCa`.
#' @param level Confidence level.
#' @param cluster Cluster variable name or vector for methods that require it.
#' @param seed Reproducible seed.
#' @param ... Additional backend arguments.
#' @return A `mixedflow_boot` object containing native output and interval estimates.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: fixed-effect parametric bootstrap
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot(m, "fixed", B = 20, seed = 1)
#'   }
#' }
#' # Example 2: variance-component bootstrap
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot(m, "variance", B = 20, seed = 2)
#'   }
#' }
#' # Example 3: BCa via confintROB
#' \dontrun{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("confintROB", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot(m, "fixed", B = 999, interval = "BCa", cluster = "subject")
#'   }
#' }
mixed_boot <- function(object,statistic=c("fixed","variance"),B=999,method=c("parametric","wild"),interval=c("percentile","BCa"),level=.95,cluster=NULL,seed=123,...) {
  statistic<-match.arg(statistic); method<-match.arg(method); interval<-match.arg(interval); m<-.mf_model(object); eng<-.mf_engine(object)
  set.seed(seed)
  if(interval=="BCa" || method=="wild" || eng=="robustlmm") {
    .mf_need("confintROB","wild or BCa mixed-model bootstrap intervals")
    dat<-if(inherits(object,"mixedflow_fit")) object$data else tryCatch(stats::model.frame(m),error=function(e) NULL)
    cl<-cluster; if(length(cluster)==1L&&is.character(cluster)&&!is.null(dat)) cl<-dat[[cluster]]
    native<-confintROB::confintROB(m,level=level,method=if(interval=="BCa") "BCa" else "boot",nsim=B,boot.type=method,clusterID=cl,...)
    return(structure(list(call=match.call(),engine="confintROB",statistic=statistic,B=B,method=method,interval=interval,level=level,native=native,seed=seed),class="mixedflow_boot"))
  }
  if(eng!="lme4") stop("Parametric percentile bootstrap is currently implemented directly for lme4 fits; use confintROB-supported objects for robust/wild/BCa inference.",call.=FALSE)
  .mf_need("lme4","parametric mixed-model bootstrap")
  fn<-.mf_boot_fun(statistic); b<-lme4::bootMer(m,FUN=fn,nsim=B,type="parametric",use.u=FALSE,...)
  alpha<-(1-level)/2; ci<-t(apply(b$t,2,stats::quantile,probs=c(alpha,1-alpha),na.rm=TRUE)); colnames(ci)<-c("lower","upper")
  # Interval rows must carry the parameter they belong to: an unnamed interval is
  # not reportable, and downstream tables cannot rebuild the association later.
  nm<-names(b$t0); if(is.null(nm)) nm<-colnames(b$t); if(is.null(nm)) nm<-paste0("parameter",seq_len(nrow(ci)))
  rownames(ci)<-nm
  structure(list(call=match.call(),engine="lme4",statistic=statistic,B=B,method=method,interval=interval,level=level,t0=b$t0,t=b$t,confint=ci,native=b,seed=seed),class="mixedflow_boot")
}

#' Bootstrap fixed effects
#' @param object Fitted mixed model.
#' @param ... Arguments passed to `mixed_boot`.
#' @return A `mixedflow_boot`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: small teaching bootstrap
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot_fixed(m, B = 20)
#'   }
#' }
#' # Example 2: 90 percent intervals
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot_fixed(m, B = 20, level = 0.9)
#'   }
#' }
#' # Example 3: reproducible seed
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_boot_fixed(m, B = 20, seed = 260818)
#'   }
#' }
mixed_boot_fixed <- function(object,...) mixed_boot(object,statistic="fixed",...)

#' Bootstrap variance components
#' @param object Fitted mixed model.
#' @param ... Arguments passed to `mixed_boot`.
#' @return A `mixedflow_boot`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: variance bootstrap
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_boot_variance(m, B = 20)
#'   }
#' }
#' # Example 2: random slope variance
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_random_slopes(d, "height", "time", "time", "subject")
#'       mixed_boot_variance(m, B = 20)
#'   }
#' }
#' # Example 3: alternative seed
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment + (1 | subject), d)
#'       mixed_boot_variance(m, B = 20, seed = 7)
#'   }
#' }
mixed_boot_variance <- function(object,...) mixed_boot(object,statistic="variance",...)

#' Bootstrap predictions from a linear mixed model
#' @param object Fitted `lmer` model.
#' @param newdata Prediction data.
#' @param B Number of bootstrap replicates.
#' @param level Confidence level.
#' @param re.form Random-effects prediction specification passed to `predict`.
#' @param seed Seed.
#' @param ... Additional `bootMer` arguments.
#' @return Prediction table with percentile intervals.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: population predictions
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       g <- data.frame(time = c(0, 5), subject = d$subject[1])
#'       mixed_boot_predict(m, g, B = 20)
#'   }
#' }
#' # Example 2: conditional predictions
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       g <- d[1:3, ]
#'       mixed_boot_predict(m, g, B = 20, re.form = NULL)
#'   }
#' }
#' # Example 3: 90 percent interval
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_boot_predict(m, d[1:4, ], B = 20, level = 0.9)
#'   }
#' }
mixed_boot_predict <- function(object,newdata,B=999,level=.95,re.form=NA,seed=123,...) {
  .mf_need("lme4","prediction bootstrap"); m<-.mf_model(object); if(.mf_engine(object)!="lme4") stop("Prediction bootstrap currently requires an lme4 fit.",call.=FALSE)
  set.seed(seed); fn<-function(x) stats::predict(x,newdata=newdata,re.form=re.form,allow.new.levels=TRUE)
  b<-lme4::bootMer(m,FUN=fn,nsim=B,type="parametric",use.u=FALSE,...); a<-(1-level)/2
  data.frame(newdata,estimate=as.numeric(b$t0),lower=apply(b$t,2,stats::quantile,a,na.rm=TRUE),upper=apply(b$t,2,stats::quantile,1-a,na.rm=TRUE),check.names=FALSE)
}
