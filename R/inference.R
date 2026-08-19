#' Estimated marginal means for mixed models
#' @param object Fitted model.
#' @param specs `emmeans` specification.
#' @param ... Additional `emmeans::emmeans` arguments.
#' @return An `emmGrid`.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: variety means
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_emmeans(m, ~variety)
#'   }
#' }
#' # Example 2: variety at nitrogen levels
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_emmeans(m, ~variety | nitrogen, at = list(nitrogen = c(0, 150)))
#'   }
#' }
#' # Example 3: response scale for GLMM
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       b <- mixed_data("binomial")
#'       m <- mixed_glmm(cbind(diseased, healthy) ~ treatment + (1 | block), b, 
#'           binomial())
#'       mixed_emmeans(m, ~treatment, type = "response")
#'   }
#' }
mixed_emmeans <- function(object,specs,...) { .mf_need("emmeans","estimated marginal means"); emmeans::emmeans(.mf_model(object),specs=specs,...) }

#' Contrasts among mixed-model estimated marginal means
#' @param object A fitted model or `emmGrid`.
#' @param specs `emmeans` specs when a fitted model is supplied.
#' @param method Contrast method.
#' @param adjust Multiplicity adjustment.
#' @param ... Additional arguments.
#' @return An `emmGrid` of contrasts.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: Tukey variety contrasts
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_contrasts(m, ~variety)
#'   }
#' }
#' # Example 2: treatment-vs-control contrasts
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       mixed_contrasts(m, ~variety, "trt.vs.ctrl")
#'   }
#' }
#' # Example 3: contrasts from precomputed EMMs
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       e <- mixed_emmeans(m, ~variety)
#'       mixed_contrasts(e, method = "pairwise")
#'   }
#' }
mixed_contrasts <- function(object,specs=NULL,method="pairwise",adjust="tukey",...) {
  .mf_need("emmeans","mixed-model contrasts"); e<-if(inherits(object,"emmGrid")) object else mixed_emmeans(object,specs,...); emmeans::contrast(e,method=method,adjust=adjust)
}

#' Compact-letter display for estimated marginal means
#' @param object Fitted model or `emmGrid`.
#' @param specs EMM specification when needed.
#' @param adjust Multiplicity adjustment.
#' @param alpha Familywise threshold.
#' @param ... Additional `emmeans::cld` arguments.
#' @return A compact-letter display. Letters encode non-rejection and should not be interpreted as proof of equality.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: variety grouping
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE) && requireNamespace("multcompView", quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       mixed_cld(m, ~variety)
#'   }
#' }
#' # Example 2: Sidak adjustment
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE) && requireNamespace("multcompView", quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       mixed_cld(m, ~variety, "sidak")
#'   }
#' }
#' # Example 3: existing EMM grid
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE) && requireNamespace("multcompView", quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       e <- mixed_emmeans(m, ~variety)
#'       mixed_cld(e)
#'   }
#' }
mixed_cld <- function(object,specs=NULL,adjust="tukey",alpha=.05,...) {
  .mf_need("emmeans","compact-letter displays"); .mf_need("multcomp","compact-letter displays"); .mf_need("multcompView","compact-letter displays"); e<-if(inherits(object,"emmGrid")) object else mixed_emmeans(object,specs); multcomp::cld(e,adjust=adjust,alpha=alpha,...)
}

#' Estimate quantitative-factor trends
#' @param object Fitted model.
#' @param specs Factor(s) over which trends are compared.
#' @param var Quantitative predictor.
#' @param ... Additional `emmeans::emtrends` arguments.
#' @return An `emmGrid` of local slopes.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: nitrogen trend by variety
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_trend(m, ~variety, "nitrogen")
#'   }
#' }
#' # Example 2: common nitrogen trend
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety + nitrogen + (1 | block), d)
#'       mixed_trend(m, ~1, "nitrogen")
#'   }
#' }
#' # Example 3: longitudinal slope by treatment
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       x <- mixed_data("longitudinal")
#'       m <- mixed_fit(height ~ treatment * time + (time | subject), x)
#'       mixed_trend(m, ~treatment, "time")
#'   }
#' }
mixed_trend <- function(object,specs,var,...) { .mf_need("emmeans","estimated marginal trends"); emmeans::emtrends(.mf_model(object),specs=specs,var=var,...) }

#' Estimate a mixed-model response curve
#' @param object Fitted model.
#' @param variable Quantitative variable.
#' @param values Values at which predictions are requested.
#' @param by Optional factor defining separate curves.
#' @param type Prediction scale.
#' @param ... Additional `emmeans` arguments.
#' @return A data frame suitable for publication-ready plotting.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: nitrogen curve
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_curve(m, "nitrogen", seq(0, 150, 25), "variety")
#'   }
#' }
#' # Example 2: longitudinal curves
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       x <- mixed_data("longitudinal")
#'       m <- mixed_fit(height ~ treatment * time + (1 | subject), x)
#'       mixed_curve(m, "time", seq(0, 5, 0.5), "treatment")
#'   }
#' }
#' # Example 3: population curve without by factor
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ nitrogen + (1 | block), d)
#'       mixed_curve(m, "nitrogen", seq(0, 150, 10))
#'   }
#' }
mixed_curve <- function(object,variable,values,by=NULL,type="response",...) {
  .mf_need("emmeans","mixed-model curves"); at<-stats::setNames(list(values),variable); specs<-if(is.null(by)) stats::as.formula(paste("~",.mf_bt(variable))) else stats::as.formula(paste("~",.mf_bt(variable),"|",paste(vapply(by,.mf_bt,character(1)),collapse="+"))); as.data.frame(emmeans::emmeans(.mf_model(object),specs=specs,at=at,type=type,...))
}

#' Design-aware omnibus inference for Gaussian mixed models
#' @param object Fitted linear mixed model.
#' @param ddf Denominator-df method: `Satterthwaite`, `Kenward-Roger`, or backend/default.
#' @param type ANOVA type passed to `lmerTest::anova` when applicable.
#' @return An ANOVA-like table.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: Satterthwaite tests
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("lmerTest", 
#'       quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_anova(m)
#'   }
#' }
#' # Example 2: Kenward-Roger tests
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("lmerTest", 
#'       quietly = TRUE) && requireNamespace("pbkrtest", quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block) + (1 | block:variety), 
#'           d)
#'       mixed_anova(m, "Kenward-Roger")
#'   }
#' }
#' # Example 3: backend likelihood table
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       m <- mixed_fit(yield ~ variety * nitrogen + (1 | block), d)
#'       mixed_anova(m, "backend")
#'   }
#' }
mixed_anova <- function(object,ddf=c("Satterthwaite","Kenward-Roger","backend"),type=3) {
  ddf<-match.arg(ddf); m<-.mf_model(object)
  if(ddf=="backend") return(as.data.frame(stats::anova(m)))
  if(!inherits(m,"lmerMod")) stop("Satterthwaite/Kenward-Roger ANOVA is currently provided for lmer models.",call.=FALSE)
  .mf_need("lmerTest","small-sample denominator-df inference")
  if(ddf=="Kenward-Roger") {
    .mf_need("pbkrtest","Kenward-Roger inference")
    if(!lme4::isREML(m)) stop("Kenward-Roger degrees of freedom require a REML fit, because the method corrects the variance of the estimated fixed effects using the REML covariance of the variance components. Refit with REML=TRUE for this test, and keep the ML fit only for likelihood comparisons of fixed effects.",call.=FALSE)
  }
  lmert<-lmerTest::as_lmerModLmerTest(m); as.data.frame(stats::anova(lmert,type=type,ddf=ddf))
}

#' Fit qualitative-by-quantitative mixed response models
#' @param data Data frame.
#' @param response Response column.
#' @param quantitative Quantitative predictor.
#' @param qualitative Optional qualitative factor.
#' @param group Random-intercept grouping factor.
#' @param degree Polynomial degree when `basis="polynomial"`.
#' @param basis `linear`, `polynomial`, or `spline`.
#' @param df Degrees of freedom for natural splines.
#' @param engine Mixed-model engine.
#' @param ... Additional arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: linear nitrogen trends by variety
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_quantitative(d, "yield", 
#'       "nitrogen", "variety", "block", basis = "linear")
#' }
#' # Example 2: quadratic nitrogen curves
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_quantitative(d, "yield", 
#'       "nitrogen", "variety", "block", degree = 2, basis = "polynomial")
#' }
#' # Example 3: natural spline trajectory
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       z <- mixed_data("longitudinal")
#'       mixed_quantitative(z, "height", "time", "treatment", "subject", basis = "spline", 
#'           df = 3)
#'   }
#' }
mixed_quantitative <- function(data,response,quantitative,qualitative=NULL,group,degree=2,basis=c("polynomial","linear","spline"),df=4,engine="lme4",...) {
  basis<-match.arg(basis); q<-.mf_bt(quantitative)
  term<-switch(basis,linear=q,polynomial=paste0("poly(",q,",",degree,",raw=TRUE)"),spline={.mf_need("splines","natural-spline quantitative mixed models"); paste0("splines::ns(",q,",df=",df,")")})
  rhs<-if(is.null(qualitative)) term else paste0(.mf_bt(qualitative)," * ",term)
  f<-stats::as.formula(paste(.mf_bt(response),"~",rhs,"+ (1|",.mf_bt(group),")")); mixed_fit(f,data,engine=engine,...)
}

#' Locate an optimum on an estimated mixed-model response curve
#' @param object Fitted model.
#' @param variable Quantitative predictor.
#' @param range Numeric search interval.
#' @param by Optional qualitative factor.
#' @param objective `max` or `min`.
#' @param n Grid resolution.
#' @param ... Additional arguments passed to `mixed_curve`.
#' @return A data frame with the grid-based optimum for each `by` group.
#' @details This is an estimand derived from a finite prediction grid, not a claim of a global mathematical optimum outside the declared range.
#' @export
#' @examples
#' d<-mixed_data("splitplot")
#' # Example 1: maximum nitrogen response
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_quantitative(d, "yield", "nitrogen", "variety", "block", degree = 2)
#'       mixed_optimum(m, "nitrogen", c(0, 150), "variety")
#'   }
#' }
#' # Example 2: minimum response
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_quantitative(d, "yield", "nitrogen", group = "block", degree = 2)
#'       mixed_optimum(m, "nitrogen", c(0, 150), objective = "min")
#'   }
#' }
#' # Example 3: finer grid
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE) && requireNamespace("emmeans", 
#'       quietly = TRUE)) {
#'       m <- mixed_quantitative(d, "yield", "nitrogen", "variety", "block", degree = 2)
#'       mixed_optimum(m, "nitrogen", c(0, 150), "variety", n = 301)
#'   }
#' }
mixed_optimum <- function(object,variable,range,by=NULL,objective=c("max","min"),n=201,...) {
  objective<-match.arg(objective); vals<-seq(range[1],range[2],length.out=n); d<-mixed_curve(object,variable,vals,by=by,...)
  ycol<-intersect(c("emmean","response","prob","rate"),names(d))[1]; if(is.na(ycol)||is.null(ycol)) stop("Could not identify the estimated-response column.",call.=FALSE)
  if(is.null(by)) {i<-if(objective=="max") which.max(d[[ycol]]) else which.min(d[[ycol]]); return(d[i,,drop=FALSE])}
  key<-interaction(d[by],drop=TRUE); idx<-split(seq_len(nrow(d)),key); do.call(rbind,lapply(idx,function(ii){j<-if(objective=="max") ii[which.max(d[[ycol]][ii])] else ii[which.min(d[[ycol]][ii])]; d[j,,drop=FALSE]}))
}
