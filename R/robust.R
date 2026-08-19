#' Fit a robust linear mixed model
#' @param formula Mixed-model formula.
#' @param data Data frame.
#' @param ... Additional `robustlmm::rlmer` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: random intercept
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) mixed_robust(height ~ treatment * 
#'       time + (1 | subject), d)
#' }
#' # Example 2: random slope
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) mixed_robust(height ~ time + 
#'       (time | subject), d)
#' }
#' # Example 3: contaminated observation
#' dc <- d
#' dc$height[1] <- dc$height[1] + 30
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) mixed_robust(height ~ time + 
#'       (1 | subject), dc)
#' }
mixed_robust <- function(formula,data,...) {
  .mf_need("robustlmm","robust linear mixed models"); fit<-.mf_fit(quote(robustlmm::rlmer),formula,data,...)
  .mf_wrap(fit,"robustlmm",match.call(),data,specification=list(estimation="robust"))
}

#' Screen influence in a robust mixed-model fit
#' @param object Robust or classical fitted model.
#' @param cluster Optional cluster column name.
#' @return Observation-level residual scores, or cluster summaries.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: observation screen
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) {
#'       m <- mixed_robust(height ~ time + (1 | subject), d)
#'       mixed_robust_influence(m)
#'   }
#' }
#' # Example 2: cluster screen
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) {
#'       m <- mixed_robust(height ~ time + (1 | subject), d)
#'       mixed_robust_influence(m, "subject")
#'   }
#' }
#' # Example 3: contaminated data
#' dc <- d
#' dc$height[1] <- dc$height[1] + 25
#' \donttest{
#'   if (requireNamespace("robustlmm", quietly = TRUE)) {
#'       m <- mixed_robust(height ~ time + (1 | subject), dc)
#'       mixed_robust_influence(m, "subject")
#'   }
#' }
mixed_robust_influence <- function(object,cluster=NULL) {
  m<-.mf_model(object); r<-as.numeric(stats::residuals(m)); s<-stats::mad(r,constant=1.4826,na.rm=TRUE); if(!is.finite(s)||s==0) s<-stats::sd(r,na.rm=TRUE)
  z<-r/s; d<-data.frame(index=seq_along(r),residual=r,robust_z=z,abs_robust_z=abs(z))
  if(is.null(cluster)) return(d[order(-d$abs_robust_z),])
  dat<-if(inherits(object,"mixedflow_fit")) object$data else stats::model.frame(m); cl<-if(length(cluster)==1L&&is.character(cluster)) dat[[cluster]] else cluster
  a<-stats::aggregate(d$abs_robust_z,list(cluster=cl),function(x)c(max=max(x,na.rm=TRUE),mean=mean(x,na.rm=TRUE))); data.frame(cluster=a$cluster,max_abs_z=vapply(a$x,`[`,numeric(1),1),mean_abs_z=vapply(a$x,`[`,numeric(1),2))
}

#' Compare classical and robust mixed-model estimates
#' @param classical Classical `lmer` or `mixedflow_fit` object.
#' @param robust Optional `rlmer` fit. If omitted, it is refitted from the classical formula and stored data.
#' @return Matched fixed-effect estimates and shifts.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: explicit pair
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("robustlmm", 
#'       quietly = TRUE)) {
#'       a <- mixed_fit(height ~ time + (1 | subject), d)
#'       b <- mixed_robust(height ~ time + (1 | subject), d)
#'       mixed_compare_robust(a, b)
#'   }
#' }
#' # Example 2: automatic robust refit from a stored mixedFlowR fit
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("robustlmm", 
#'       quietly = TRUE)) {
#'       a <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_compare_robust(a)
#'   }
#' }
#' # Example 3: contaminated model
#' dc <- d
#' dc$height[1] <- dc$height[1] + 25
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("robustlmm", 
#'       quietly = TRUE)) {
#'       a <- mixed_fit(height ~ time + (1 | subject), dc)
#'       b <- mixed_robust(height ~ time + (1 | subject), dc)
#'       mixed_compare_robust(a, b)
#'   }
#' }
mixed_compare_robust <- function(classical,robust=NULL) {
  if (is.null(robust)) {
    .mf_need("robustlmm","automatic robust sensitivity refitting")
    if (!inherits(classical,"mixedflow_fit") || is.null(classical$data)) stop("Automatic robust refitting requires a mixedflow_fit with stored data.",call.=FALSE)
    robust<-mixed_robust(stats::formula(.mf_model(classical)),classical$data)
  }
  a<-.mf_tidy_fixed(classical); b<-.mf_tidy_fixed(robust)
  z<-merge(a[,c("term","estimate")],b[,c("term","estimate")],by="term",suffixes=c(".classical",".robust"))
  z$shift<-z$estimate.robust-z$estimate.classical
  z$relative_shift<-ifelse(abs(z$estimate.classical)>.Machine$double.eps,z$shift/abs(z$estimate.classical),NA_real_)
  z
}

#' Flag observations or clusters for contamination sensitivity
#' @param object Fitted model.
#' @param cluster Optional cluster column.
#' @param threshold Absolute robust residual threshold.
#' @return A sensitivity table. Rows are flagged for review, never automatically deleted.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: observation flags
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_contamination(m)
#'   }
#' }
#' # Example 2: stricter threshold
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_contamination(m, threshold = 3)
#'   }
#' }
#' # Example 3: cluster aggregation
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_contamination(m, "subject")
#'   }
#' }
mixed_contamination <- function(object,cluster=NULL,threshold=2.5) {
  d<-mixed_robust_influence(object,cluster=NULL); d$flag<-d$abs_robust_z>=threshold
  if(is.null(cluster)) return(d)
  dat<-if(inherits(object,"mixedflow_fit")) object$data else stats::model.frame(.mf_model(object)); cl<-if(is.character(cluster)) dat[[cluster]] else cluster
  stats::aggregate(cbind(flag=d$flag,abs_robust_z=d$abs_robust_z),list(cluster=cl),function(x) if(is.logical(x)) sum(x) else max(x,na.rm=TRUE))
}

#' Cluster-robust covariance for fixed effects
#' @param object Supported mixed model.
#' @param cluster Cluster variable name or vector.
#' @param type `clubSandwich` covariance type, commonly `CR2`.
#' @return A cluster-robust covariance matrix.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: CR2 by subject
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("clubSandwich", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_robust_vcov(m, "subject")
#'   }
#' }
#' # Example 2: CR1 by subject
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("clubSandwich", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_robust_vcov(m, "subject", "CR1")
#'   }
#' }
#' # Example 3: supplied cluster vector
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("clubSandwich", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_robust_vcov(m, d$subject)
#'   }
#' }
mixed_robust_vcov <- function(object,cluster,type="CR2") {
  .mf_need("clubSandwich","cluster-robust covariance estimation"); m<-.mf_model(object); dat<-if(inherits(object,"mixedflow_fit")) object$data else stats::model.frame(m); cl<-if(length(cluster)==1L&&is.character(cluster)) dat[[cluster]] else cluster
  clubSandwich::vcovCR(m,cluster=cl,type=type)
}
