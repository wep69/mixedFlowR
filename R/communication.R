#' Create publication-oriented tables from mixed-model results
#' @param object Fitted model or mixedFlowR result.
#' @param component Table component: fixed effects, random effects, ANOVA, EMMs, bootstrap, diagnostics, or comparison.
#' @param format Output format: `data.frame`, `markdown`, `latex`, `html`, or `flextable`.
#' @param digits Number of displayed digits for rendered tables.
#' @param caption Optional table caption.
#' @param ... Arguments passed to the relevant extraction function.
#' @return A data frame with class `mixedflow_table`, a `knitr_kable`, or a `flextable`, depending on `format`.
#' @details The data-frame output remains the authoritative numerical object. Rendered formats are communication layers and do not change estimates or uncertainty.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: fixed effects as a reusable data frame
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_table(m, "fixed")
#'   }
#' }
#' # Example 2: variance components rendered for Markdown
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("knitr", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_table(m, "random", format = "markdown")
#'   }
#' }
#' # Example 3: marginal means table
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_table(m, "emmeans", specs = ~treatment)
#'   }
#' }
mixed_table <- function(object,component=c("fixed","random","anova","emmeans","bootstrap","diagnostics","comparison"),format=c("data.frame","markdown","latex","html","flextable"),digits=3,caption=NULL,...) {
  component<-match.arg(component); format<-match.arg(format)
  z<-switch(component,
    fixed=.mf_tidy_fixed(object),
    random=.mf_tidy_varcorr(object),
    anova=as.data.frame(stats::anova(.mf_model(object))),
    emmeans=as.data.frame(do.call(mixed_emmeans,c(list(object=object),list(...)))),
    bootstrap={if(!inherits(object,"mixedflow_boot")) stop("A mixedflow_boot object is required.",call.=FALSE); if(!is.null(object$confint)) {ci<-object$confint; nm<-rownames(ci); if(is.null(nm)) nm<-names(object$t0); if(is.null(nm)) nm<-paste0("parameter",seq_len(nrow(ci))); data.frame(parameter=nm,ci,row.names=NULL)} else as.data.frame(object$native)},
    diagnostics={x<-if(inherits(object,"mixedflow_diagnostics")) object else mixed_diagnose(object); data.frame(engine=x$engine,converged=x$converged,singular=x$singular,message=paste(x$convergence_messages,collapse="; "))},
    comparison=as.data.frame(object))
  z<-as.data.frame(z,check.names=FALSE)
  if(is.null(caption)) caption<-paste("mixedFlowR",component,"table")
  class(z)<-c("mixedflow_table",class(z)); attr(z,"caption")<-caption
  if(format=="data.frame") return(z)
  if(format=="flextable") {
    .mf_need("flextable","Word-ready table rendering")
    ft<-flextable::flextable(z)
    num<-names(z)[vapply(z,is.numeric,logical(1))]
    if(length(num)) ft<-flextable::colformat_double(ft,j=num,digits=digits)
    return(flextable::set_caption(ft,caption=caption))
  }
  .mf_need("knitr",paste(format,"table rendering"))
  knitr::kable(z,format=format,digits=digits,caption=caption)
}

#' Create publication-ready mixed-model graphics
#' @param object Fitted model or mixedFlowR result.
#' @param type Plot type: `raw`, `residuals`, `fixed`, `random`, `curve`, `covariance`, `bootstrap`, or `spatial`.
#' @param x Optional quantitative x-variable for raw or curve plots.
#' @param y Optional response variable for raw plots. If omitted, the fitted response is inferred when possible.
#' @param by Optional grouping factor for raw or curve plots.
#' @param values Prediction values for curves.
#' @param show_data For curve plots, overlay the observed response whenever stored model data and the response can be identified.
#' @param file Optional output file. Use `.pdf` or `.svg` for vector output and `.tiff`/`.png` for raster output.
#' @param width,height Output dimensions in inches.
#' @param dpi Raster resolution; the default is 600 dpi.
#' @param ... Additional arguments passed to underlying helpers.
#' @return A `ggplot` object, after optional export.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: observed measurements
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_plot(m, "raw", x = "time", by = "treatment")
#'   }
#' }
#' # Example 2: residual diagnostic
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(height ~ time + (1 | subject), d)
#'       mixed_plot(m, "residuals")
#'   }
#' }
#' # Example 3: treatment curves with observed data and model uncertainty
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), d)
#'       mixed_plot(m, "curve", x = "time", by = "treatment", values = seq(0, 5, 
#'           0.25))
#'   }
#' }
mixed_plot <- function(object,type=c("raw","residuals","fixed","random","curve","covariance","bootstrap","spatial"),x=NULL,y=NULL,by=NULL,values=NULL,show_data=TRUE,file=NULL,width=7,height=5,dpi=600,...) {
  type<-match.arg(type)
  stored<-if(inherits(object,"mixedflow_fit")) object$data else NULL
  p<-switch(type,
    raw={
      if(is.null(stored)) stop("Raw-data plotting requires a mixedflow_fit with stored data.",call.=FALSE)
      if(is.null(x)) stop("Raw-data plots require x.",call.=FALSE)
      if(is.null(y)) y<-.mf_response_name(object)
      if(is.null(y)||!all(c(x,y)%in%names(stored))) stop("Could not identify x and response columns in stored data.",call.=FALSE)
      p0<-ggplot2::ggplot(stored,ggplot2::aes(x=.data[[x]],y=.data[[y]]))
      if(!is.null(by)) p0<-p0+ggplot2::geom_point(ggplot2::aes(colour=.data[[by]]),alpha=.65) else p0<-p0+ggplot2::geom_point(alpha=.65)
      p0+.mf_theme()+ggplot2::labs(x=x,y=y,colour=by)
    },
    residuals={d<-mixed_diagnose(object)$residuals; ggplot2::ggplot(d,ggplot2::aes(x=.data$fitted,y=.data$residual))+ggplot2::geom_point(alpha=.65)+ggplot2::geom_hline(yintercept=0,lty=2)+ggplot2::geom_smooth(method="loess",se=FALSE)+.mf_theme()+ggplot2::labs(x="Fitted value",y="Residual")},
    fixed={d<-.mf_tidy_fixed(object); if(!"std.error"%in%names(d)) d$std.error<-NA_real_; ggplot2::ggplot(d,ggplot2::aes(x=.data$estimate,y=stats::reorder(.data$term,.data$estimate)))+ggplot2::geom_point()+ggplot2::geom_errorbarh(ggplot2::aes(xmin=.data$estimate-1.96*.data$std.error,xmax=.data$estimate+1.96*.data$std.error),height=.15)+.mf_theme()+ggplot2::labs(x="Estimate (95% Wald interval)",y=NULL)},
    random={m<-.mf_model(object); eng<-.mf_engine(object); if(!eng%in%c("lme4","robustlmm","nlme","glmmTMB")) stop("Random-effect plotting is unavailable for this engine.",call.=FALSE); rr<-if(eng%in%c("lme4","robustlmm")) lme4::ranef(m) else if(eng=="nlme") nlme::ranef(m) else glmmTMB::ranef(m)$cond; g<-names(rr)[1]; q<-rr[[1]]; d<-data.frame(level=rownames(q),effect=q[,1]); ggplot2::ggplot(d,ggplot2::aes(x=.data$effect,y=stats::reorder(.data$level,.data$effect)))+ggplot2::geom_point()+ggplot2::geom_vline(xintercept=0,lty=2)+.mf_theme()+ggplot2::labs(x="Conditional random-effect estimate",y=g)},
    curve={
      if(is.null(x)||is.null(values)) stop("Curve plots require x and values.",call.=FALSE)
      d<-mixed_curve(object,x,values,by=by,...)
      candidates<-c("emmean","response","prob","rate","estimate")
      ycol<-intersect(candidates,names(d))[1]
      if(!length(ycol)||is.na(ycol)) {nums<-names(d)[vapply(d,is.numeric,logical(1))]; ycol<-setdiff(nums,x)[1]}
      if(!length(ycol)||is.na(ycol)) stop("Could not identify the estimated response column.",call.=FALSE)
      lo<-intersect(c("lower.CL","asymp.LCL","lower.HPD"),names(d))[1]; hi<-intersect(c("upper.CL","asymp.UCL","upper.HPD"),names(d))[1]
      p0<-ggplot2::ggplot(d,ggplot2::aes(x=.data[[x]],y=.data[[ycol]]))
      if(!is.null(by)) p0<-p0+ggplot2::geom_line(ggplot2::aes(colour=.data[[by]],group=.data[[by]]),linewidth=.7) else p0<-p0+ggplot2::geom_line(linewidth=.7)
      if(length(lo)&&length(hi)&&!is.na(lo)&&!is.na(hi)) {
        if(!is.null(by)) p0<-p0+ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[lo]],ymax=.data[[hi]],fill=.data[[by]],group=.data[[by]]),alpha=.15,colour=NA) else p0<-p0+ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[lo]],ymax=.data[[hi]]),alpha=.15)
      }
      if(isTRUE(show_data)&&!is.null(stored)) {
        resp<-.mf_response_name(object)
        if(!is.null(resp)&&all(c(x,resp)%in%names(stored))) {
          if(!is.null(by)&&by%in%names(stored)) p0<-p0+ggplot2::geom_point(data=stored,ggplot2::aes(x=.data[[x]],y=.data[[resp]],colour=.data[[by]]),inherit.aes=FALSE,alpha=.3) else p0<-p0+ggplot2::geom_point(data=stored,ggplot2::aes(x=.data[[x]],y=.data[[resp]]),inherit.aes=FALSE,alpha=.3)
        }
      }
      p0+.mf_theme()+ggplot2::labs(x=x,y="Estimated response",colour=by,fill=by)
    },
    covariance=mixed_covariance_plot(object,...),
    bootstrap={if(!inherits(object,"mixedflow_boot")||is.null(object$t)) stop("A direct bootstrap object with stored replicates is required.",call.=FALSE); d<-data.frame(value=as.vector(object$t),parameter=rep(seq_len(ncol(object$t)),each=nrow(object$t))); ggplot2::ggplot(d,ggplot2::aes(x=.data$value))+ggplot2::geom_histogram(bins=30)+ggplot2::facet_wrap(~parameter,scales="free")+.mf_theme()+ggplot2::labs(x="Bootstrap estimate",y="Frequency")},
    spatial=mixed_spatial_surface(object,...)$plot)
  if(!is.null(file)) ggplot2::ggsave(filename=file,plot=p,width=width,height=height,dpi=dpi,units="in")
  p
}

#' Interactive teaching tour of mixed-model concepts
#' @param topic Teaching topic.
#' @param run If TRUE, fit lightweight examples when the required engine is installed.
#' @return A `mixedflow_tour` object containing a teaching sequence and optional analysis artifacts.
#' @export
#' @examples
#' # Example 1: split-plot tour
#' mixed_tour("splitplot")
#' # Example 2: partial-pooling tour
#' mixed_tour("random_effects")
#' # Example 3: covariance tour
#' mixed_tour("covariance")
mixed_tour <- function(topic=c("splitplot","random_effects","covariance","robust","gamlss","bayes","spatial","met"),run=FALSE) {
  topic<-match.arg(topic)
  steps<-switch(topic,
    splitplot=c("Identify whole-plot and subplot experimental units","Show the two error strata","Fit legacy ANOVA","Represent the same design as a mixed model","Compare estimands and assumptions"),
    random_effects=c("Fit complete-pooling fixed regression","Add a random intercept","Add random slopes when scientifically justified","Visualize shrinkage/partial pooling","Inspect singularity and predictive implications"),
    covariance=c("Plot within-unit dependence","Distinguish residual from structured random covariance","Fit scientifically plausible structures","Inspect estimated correlation","Compare candidates without selecting by AIC alone"),
    robust=c("Fit classical LMM","Screen residual and cluster influence","Fit robust LMM","Compare estimates and variance components","Report sensitivity without automatic deletion"),
    gamlss=c("Choose a response distribution","Model location","Add hierarchical dependence","Model scale/shape if scientifically motivated","Use quantile residuals and conditional quantiles"),
    bayes=c("Specify the hierarchical likelihood","Inspect prior classes","Run prior predictive checks","Fit posterior","Use posterior predictive checks and LOO"),
    spatial=c("Map raw field observations","Fit a 2-D P-spline trend","Inspect residual spatial pattern","Obtain adjusted genotype effects","Export field and effect figures"),
    met=c("Audit genotype-environment replication","Fit baseline GxE random effects","Model heterogeneous covariance","Fit reduced-rank factor analytic structure","Interpret stability and borrowing across environments"))
  artifact<-NULL
  if(run && topic=="splitplot") artifact<-fit_legacy_splitplot(mixed_data("splitplot"),"yield","block","variety","nitrogen")
  structure(list(topic=topic,steps=data.frame(step=seq_along(steps),instruction=steps),artifact=artifact),class="mixedflow_tour")
}

#' Write a reproducible Markdown analysis report skeleton
#' @param object Fitted mixedFlowR object.
#' @param file Output Markdown file.
#' @param title Report title.
#' @return Invisibly, the report path.
#' @export
#' @examples
#' # Example 1: report path from a legacy fit
#' d <- mixed_data("splitplot")
#' m <- fit_legacy_splitplot(d, "yield", "block", "variety", "nitrogen")
#' tf <- tempfile(fileext = ".md")
#' mixed_report(m, tf)
#' # Example 2: custom title
#' tf<-tempfile(fileext=".md"); mixed_report(m,tf,"Split-plot agronomic report")
#' # Example 3: inspect generated text
#' tf<-tempfile(fileext=".md"); mixed_report(m,tf); readLines(tf,n=5)
mixed_report <- function(object,file,title="mixedFlowR analysis report") {
  eng<-.mf_engine(object); lines<-c(paste0("# ",title),"",paste0("Generated: ",Sys.time()),paste0("Engine: `",eng,"`"),"","## Model specification","",paste0("Call: `",paste(deparse(if(inherits(object,"mixedflow_fit")) object$call else match.call()),collapse=" "),"`"),"","## Required interpretation workflow","","1. Confirm experimental units and randomization.","2. Inspect convergence, singularity, residual dependence and distributional assumptions.","3. Report estimates with uncertainty and scientifically relevant contrasts.","4. Display observed data together with model estimates whenever feasible.","5. Record package versions, seeds and sensitivity analyses.")
  writeLines(lines,file); invisible(normalizePath(file,mustWork=FALSE))
}
