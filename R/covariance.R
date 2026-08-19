#' Specify a covariance structure
#' @param type Covariance structure.
#' @param time Time or ordered coordinate variable.
#' @param group Grouping variable.
#' @param spatial Optional two-dimensional coordinate names.
#' @return A `mixedflow_covariance` specification.
#' @export
#' @examples
#' # Example 1: discrete AR(1)
#' mixed_covariance("ar1", "time", "subject")
#' # Example 2: continuous AR(1)
#' mixed_covariance("car1", "day", "plant")
#' # Example 3: Matern spatial structure
#' mixed_covariance("mat", group="field", spatial=c("row","col"))
mixed_covariance <- function(type=c("ar1","car1","cs","arma","toep","us","diag","ou","exp","gau","mat"), time=NULL, group=NULL, spatial=NULL) {
  type <- match.arg(type)
  if (type %in% c("ar1","car1","cs","arma","toep","us","diag","ou") && is.null(time)) stop("A time/order variable is required for this covariance structure.", call.=FALSE)
  if (is.null(group)) stop("A grouping variable is required.", call.=FALSE)
  if (type %in% c("exp","gau","mat") && (is.null(spatial) || length(spatial)!=2L)) stop("Spatial structures require two coordinate variables.", call.=FALSE)
  structure(list(type=type,time=time,group=group,spatial=spatial),class="mixedflow_covariance")
}

#' Fit a heterogeneous-residual linear mixed model
#' @param formula Fixed-effect formula.
#' @param data Data frame.
#' @param random `nlme` random-effects formula.
#' @param variance_group Factor defining residual variance strata.
#' @param correlation Optional `nlme` correlation structure.
#' @param ... Additional `nlme::lme` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: treatment-specific residual variance
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_heterogeneity(height ~ 
#'       treatment * time, d, ~1 | subject, "treatment")
#' }
#' # Example 2: time-specific residual variance
#' d$timef <- factor(d$time)
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_heterogeneity(height ~ 
#'       treatment * time, d, ~1 | subject, "timef")
#' }
#' # Example 3: heterogeneity plus AR1
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_heterogeneity(height ~ 
#'       treatment * time, d, ~1 | subject, "treatment", nlme::corAR1(form = ~time | 
#'       subject))
#' }
mixed_heterogeneity <- function(formula,data,random,variance_group,correlation=NULL,...) {
  .mf_need("nlme","heterogeneous residual mixed models")
  vf <- stats::as.formula(paste("~ 1 |", .mf_bt(variance_group)))
  fit <- .mf_fit(quote(nlme::lme),formula,data,formula_arg="fixed",random=random,weights=nlme::varIdent(form=vf),correlation=correlation,...)
  .mf_wrap(fit,"nlme",match.call(),data,specification=list(variance_group=variance_group,correlation=correlation))
}

#' Fit a discrete AR(1) mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param time Ordered time variable.
#' @param group Grouping variable.
#' @param engine `nlme`, `lme4`, or `glmmTMB`.
#' @param family Response family for `glmmTMB`/`lme4`.
#' @param ... Additional backend arguments.
#' @return A `mixedflow_fit`.
#' @details With `nlme`, AR(1) is a residual correlation. With `lme4` or `glmmTMB`, the structured term represents a structured random component and should not be interpreted as identical to a residual AR(1).
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: residual AR1
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_ar1(d, "height", "treatment*time", 
#'       "time", "subject")
#' }
#' # Example 2: lme4 structured component
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_ar1(d, "height", "treatment*time", 
#'       "time", "subject", engine = "lme4")
#' }
#' # Example 3: glmmTMB structured component
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_ar1(d, "height", "treatment*time", 
#'       "time", "subject", engine = "glmmTMB")
#' }
mixed_ar1 <- function(data,response,fixed,time,group,engine=c("nlme","lme4","glmmTMB"),family=stats::gaussian(),...) {
  engine<-match.arg(engine)
  if(engine=="nlme") {
    .mf_need("nlme","residual AR(1) models")
    cf<-stats::as.formula(paste("~",.mf_bt(time),"|",.mf_bt(group)))
    rf<-stats::as.formula(paste("~1|",.mf_bt(group)))
    fit<-.mf_fit(quote(nlme::lme),.mf_fixed_formula(response,fixed),data,formula_arg="fixed",random=rf,correlation=nlme::corAR1(form=cf),...)
    return(.mf_wrap(fit,"nlme",match.call(),data,specification=list(covariance="AR1",level="residual")))
  }
  f<-.mf_formula_with_struct(response,fixed,time,group,"ar1",engine)
  mixed_fit(f,data,family=family,engine=engine,REML=identical(family$family,"gaussian"),...)
}

#' Fit a continuous-time AR(1) mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param time Numeric time variable, including irregular spacing.
#' @param group Grouping variable.
#' @param ... Additional `nlme::lme` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: regular observations
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_car1(d, "height", "treatment*time", 
#'       "time", "subject")
#' }
#' # Example 2: irregular subset
#' di <- d[!(d$subject == "S01" & d$time == 2), ]
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_car1(di, "height", "treatment*time", 
#'       "time", "subject")
#' }
#' # Example 3: reduced fixed model
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_car1(d, "height", "time", 
#'       "time", "subject")
#' }
mixed_car1 <- function(data,response,fixed,time,group,...) {
  .mf_need("nlme","continuous-time AR(1) models")
  cf<-stats::as.formula(paste("~",.mf_bt(time),"|",.mf_bt(group))); rf<-stats::as.formula(paste("~1|",.mf_bt(group)))
  fit<-.mf_fit(quote(nlme::lme),.mf_fixed_formula(response,fixed),data,formula_arg="fixed",random=rf,correlation=nlme::corCAR1(form=cf),...)
  .mf_wrap(fit,"nlme",match.call(),data,specification=list(covariance="CAR1",level="residual"))
}

#' Fit a Toeplitz structured mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param time Ordered time variable.
#' @param group Grouping variable.
#' @param family `glmmTMB` family.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Three measurement occasions are enough to show the banded structure and keep
#' # the example fast; the full profile is analysed in the covariance vignette.
#' d<-d[d$time %in% c(0,1,2),]
#' # Example 1: Gaussian Toeplitz
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_toeplitz(d, "height", 
#'       "treatment*time", "time", "subject")
#' }
#' # Example 2: simpler mean
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_toeplitz(d, "height", 
#'       "time", "time", "subject")
#' }
#' # Example 3: counts on an ordered visit factor
#' c <- mixed_data("counts")
#' c <- c[c$time %in% sort(unique(c$time))[1:3],]
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_toeplitz(c, "count", 
#'       "treatment*time", "time", "plot", glmmTMB::nbinom2())
#' }
mixed_toeplitz <- function(data,response,fixed,time,group,family=stats::gaussian(),...) {
  .mf_need("glmmTMB","Toeplitz covariance models")
  f<-.mf_formula_with_struct(response,fixed,time,group,"toep","glmmTMB")
  mixed_fit(f,data,family=family,engine="glmmTMB",REML=identical(family$family,"gaussian"),...)
}

#' Compare candidate covariance structures
#' @param ... Fitted mixed models.
#' @return A data frame of likelihood-based descriptors, without automatically declaring a winner.
#' @export
#' @examples
#' # Example 1: empty comparison is valid metadata
#' mixed_covariance_compare()
#' # Example 2: compare two stored candidates
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       m1 <- mixed_ar1(d, "height", "time", "time", "subject")
#'       m2 <- mixed_car1(d, "height", "time", "time", "subject")
#'       mixed_covariance_compare(m1, m2)
#'   }
#' }
#' # Example 3: named candidates
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       a <- mixed_ar1(d, "height", "time", "time", "subject")
#'       mixed_covariance_compare(AR1 = a)
#'   }
#' }
mixed_covariance_compare <- function(...) {
  xs<-list(...); if(!length(xs)) return(data.frame())
  nm<-names(xs); if(is.null(nm)) nm<-rep("",length(xs)); nm[nm==""]<-paste0("model",which(nm==""))
  out<-lapply(seq_along(xs),function(i){m<-.mf_model(xs[[i]]); data.frame(model=nm[i],engine=.mf_engine(xs[[i]]),AIC=tryCatch(stats::AIC(m),error=function(e) NA_real_),BIC=tryCatch(stats::BIC(m),error=function(e) NA_real_),logLik=tryCatch(as.numeric(stats::logLik(m)),error=function(e) NA_real_))})
  do.call(rbind,out)
}

#' Fit a spatiotemporal structured mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param time Numeric or ordered time variable.
#' @param group Grouping factor.
#' @param structure `ou` for irregular continuous time or `ar1` for ordered visits.
#' @param family `glmmTMB` family.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: OU component for time
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatiotemporal(d, "height", 
#'       "treatment*time", "time", "subject", "ou")
#' }
#' # Example 2: ordered AR1 component
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatiotemporal(d, "height", 
#'       "time", "time", "subject", "ar1")
#' }
#' # Example 3: non-Gaussian repeated counts
#' c <- mixed_data("counts")
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatiotemporal(c, "count", 
#'       "treatment*time", "time", "plot", "ou", glmmTMB::nbinom2())
#' }
mixed_spatiotemporal <- function(data,response,fixed,time,group,structure=c("ou","ar1"),family=stats::gaussian(),...) {
  .mf_need("glmmTMB","spatiotemporal covariance models"); structure<-match.arg(structure)
  dat<-data
  dat$.mf_time<-if (structure=="ou") glmmTMB::numFactor(dat[[time]]) else factor(dat[[time]], ordered=TRUE)
  base<-if(inherits(fixed,"formula")) paste(deparse(fixed[[3]]),collapse=" ") else fixed
  term<-paste0(structure,"(.mf_time + 0 | ",.mf_bt(group),")")
  f<-stats::as.formula(paste(.mf_bt(response),"~",base,"+",term)); environment(f)<-asNamespace("glmmTMB")
  fit<-.mf_fit(quote(glmmTMB::glmmTMB),f,dat,family=family,...)
  .mf_wrap(fit,"glmmTMB",match.call(),dat,specification=list(covariance=structure,time=time,group=group))
}

#' Fit a spatial covariance mixed model
#' @param data Data frame.
#' @param response Response column.
#' @param fixed Fixed-effect RHS text.
#' @param x,y Numeric spatial coordinates.
#' @param structure `exp`, `gau`, or `mat`.
#' @param family `glmmTMB` family.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' s<-mixed_data("spatial")
#' # A 6 x 6 corner of the trial keeps these examples fast; the complete
#' # 12 x 18 field is analysed in the spatial field-trial vignette.
#' s<-s[s$row<=6 & s$col<=6,]
#' # Example 1: exponential spatial covariance
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatial_covariance(s, 
#'       "yield", "genotype+block", "col", "row", "exp")
#' }
#' # Example 2: Gaussian spatial covariance
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatial_covariance(s, 
#'       "yield", "genotype", "col", "row", "gau")
#' }
#' # Example 3: Matern spatial covariance
#' \donttest{
#'   if (requireNamespace("glmmTMB", quietly = TRUE)) mixed_spatial_covariance(s, 
#'       "yield", "genotype+block", "col", "row", "mat")
#' }
mixed_spatial_covariance <- function(data,response,fixed,x,y,structure=c("exp","gau","mat"),family=stats::gaussian(),...) {
  .mf_need("glmmTMB","spatial covariance models"); structure<-match.arg(structure); dat<-data
  dat$.mf_pos<-glmmTMB::numFactor(dat[[x]],dat[[y]]); dat$.mf_spatial_group<-factor(1)
  base<-if(inherits(fixed,"formula")) paste(deparse(fixed[[3]]),collapse=" ") else fixed
  term<-paste0(structure,"(.mf_pos + 0 | .mf_spatial_group)")
  f<-stats::as.formula(paste(.mf_bt(response),"~",base,"+",term)); environment(f)<-asNamespace("glmmTMB")
  fit<-.mf_fit(quote(glmmTMB::glmmTMB),f,dat,family=family,...)
  .mf_wrap(fit,"glmmTMB",match.call(),dat,specification=list(covariance=structure,coordinates=c(x,y)))
}

#' Build a regular prediction grid for repeated measurements
#' @param data Data frame.
#' @param time Numeric variable.
#' @param by Optional grouping variables retained at observed levels.
#' @param n Number of grid points.
#' @return A data frame.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: time grid
#' mixed_temporal_grid(d,"time",n=20)
#' # Example 2: separate treatment curves
#' mixed_temporal_grid(d,"time","treatment",20)
#' # Example 3: retain two grouping variables
#' mixed_temporal_grid(d,"time",c("treatment","block"),10)
mixed_temporal_grid <- function(data,time,by=NULL,n=100) {
  if(!time %in% names(data)) stop("Unknown time variable.",call.=FALSE)
  tg<-seq(min(data[[time]],na.rm=TRUE),max(data[[time]],na.rm=TRUE),length.out=n)
  if(is.null(by)) return(stats::setNames(data.frame(tg),time))
  lev<-lapply(by,function(v) unique(data[[v]])); names(lev)<-by; g<-do.call(expand.grid,c(list(tg),lev,stringsAsFactors=FALSE)); names(g)[1]<-time; g
}

#' Plot an estimated random-effect covariance matrix
#' @param object Fitted model.
#' @param group Optional grouping factor.
#' @param correlation If TRUE, convert covariance to correlation.
#' @return A publication-ready `ggplot` object.
#' @export
#' @examples
#' # Example 1: matrix utility from a fitted model
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       m <- mixed_random_slopes(d, "height", "treatment * time", "time", "subject")
#'       mixed_covariance_plot(m)
#'   }
#' }
#' # Example 2: explicitly identify group
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       m <- mixed_random_slopes(d, "height", "time", "time", "subject")
#'       mixed_covariance_plot(m, "subject")
#'   }
#' }
#' # Example 3: show covariance rather than correlation
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       d <- mixed_data("longitudinal")
#'       m <- mixed_random_slopes(d, "height", "time", "time", "subject")
#'       mixed_covariance_plot(m, correlation = FALSE)
#'   }
#' }
mixed_covariance_plot <- function(object,group=NULL,correlation=TRUE) {
  z<-.mf_varcorr_matrix(object,group); if(correlation) z<-stats::cov2cor(z)
  d<-as.data.frame(as.table(z),stringsAsFactors=FALSE); names(d)<-c("row","column","value")
  ggplot2::ggplot(d,ggplot2::aes(x=.data$column,y=.data$row,fill=.data$value))+ggplot2::geom_tile()+ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",.data$value)),size=3)+ggplot2::scale_fill_viridis_c()+ggplot2::coord_equal()+.mf_theme()+ggplot2::labs(x=NULL,y=NULL,fill=if(correlation) "Correlation" else "Covariance")
}
