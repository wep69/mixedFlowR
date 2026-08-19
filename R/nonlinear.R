#' Fit a nonlinear mixed model through an established engine
#' @param model Nonlinear model formula accepted by the selected engine.
#' @param data Data frame.
#' @param engine `nlme`, `lme4`, or `brms`.
#' @param fixed Fixed-effects specification for `nlme`.
#' @param random Random-effects specification for `nlme`.
#' @param groups Grouping specification for `nlme` when needed.
#' @param start Starting values.
#' @param ... Additional backend arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' g<-mixed_data("growth")
#' # Example 1: nlme logistic growth
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) {
#'       st <- stats::getInitial(biomass ~ SSlogis(time, Asym, xmid, scal), g)
#'       mixed_nonlinear(biomass ~ SSlogis(time, Asym, xmid, scal), g, "nlme", fixed = Asym + 
#'           xmid + scal ~ 1, random = Asym ~ 1, groups = ~plant, start = st)
#'   }
#' }
#' # Example 2: lme4 nlmer logistic growth
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) {
#'       st <- stats::getInitial(biomass ~ SSlogis(time, Asym, xmid, scal), g)
#'       mixed_nonlinear(biomass ~ SSlogis(time, Asym, xmid, scal) ~ (Asym | plant), 
#'           g, "lme4", start = st)
#'   }
#' }
#' # Example 3: Bayesian nonlinear formula supplied by the user
#' \dontrun{
#'   mixed_nonlinear(brms::bf(biomass ~ Asym/(1 + exp((xmid - time)/scal)), Asym ~ 
#'       1 + (1 | plant), xmid ~ 1, scal ~ 1, nl = TRUE), g, "brms")
#' }
mixed_nonlinear <- function(model,data,engine=c("nlme","lme4","brms"),fixed=NULL,random=NULL,groups=NULL,start=NULL,...) {
  engine<-match.arg(engine)
  if(engine=="nlme") { .mf_need("nlme","nonlinear mixed models"); fit<-.mf_fit(quote(nlme::nlme),model,data,formula_arg="model",fixed=fixed,random=random,groups=groups,start=start,...); return(.mf_wrap(fit,"nlme",match.call(),data,specification=list(nonlinear=TRUE))) }
  if(engine=="lme4") { .mf_need("lme4","nonlinear mixed models"); fit<-.mf_fit(quote(lme4::nlmer),model,data,start=start,...); return(.mf_wrap(fit,"lme4",match.call(),data,specification=list(nonlinear=TRUE))) }
  .mf_need("brms","Bayesian nonlinear mixed models"); fit<-brms::brm(formula=model,data=data,...); .mf_wrap(fit,"brms",match.call(),data,specification=list(nonlinear=TRUE))
}

#' Fit a logistic or Gompertz growth mixed model
#' @param data Data frame.
#' @param response,time Response and time columns.
#' @param group Experimental-unit grouping column.
#' @param model `logistic` or `gompertz`.
#' @param start Optional starting values; otherwise obtained from the pooled self-start model.
#' @param ... Additional `nlme` arguments.
#' @return A nonlinear `mixedflow_fit`.
#' @export
#' @examples
#' g<-mixed_data("growth")
#' # Example 1: logistic growth
#' \donttest{if(requireNamespace("nlme",quietly=TRUE)) mixed_growth(g,"biomass","time","plant")}
#' # Example 2: Gompertz growth
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_growth(g, "biomass", "time", 
#'       "plant", "gompertz")
#' }
#' # Example 3: one treatment subset
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_growth(subset(g, treatment == 
#'       "control"), "biomass", "time", "plant")
#' }
mixed_growth <- function(data,response,time,group,model=c("logistic","gompertz"),start=NULL,...) {
  .mf_need("nlme","nonlinear growth mixed models"); model<-match.arg(model); dat<-data
  if(model=="logistic") {
    f<-stats::as.formula(paste(.mf_bt(response),"~ SSlogis(",.mf_bt(time),", Asym, xmid, scal)")); if(is.null(start)) start<-stats::getInitial(f,dat)
    fit<-.mf_fit(quote(nlme::nlme),f,dat,formula_arg="model",fixed=Asym+xmid+scal~1,random=Asym~1,groups=stats::as.formula(paste("~",.mf_bt(group))),start=start,...)
  } else {
    f<-stats::as.formula(paste(.mf_bt(response),"~ SSgompertz(",.mf_bt(time),", Asym, b2, b3)")); if(is.null(start)) start<-stats::getInitial(f,dat)
    fit<-.mf_fit(quote(nlme::nlme),f,dat,formula_arg="model",fixed=Asym+b2+b3~1,random=Asym~1,groups=stats::as.formula(paste("~",.mf_bt(group))),start=start,...)
  }
  .mf_wrap(fit,"nlme",match.call(),data,specification=list(growth_model=model,group=group))
}

#' Fit a four-parameter dose-response mixed model
#' @param data Data frame.
#' @param response,dose Response and quantitative dose columns.
#' @param group Experimental-unit grouping column.
#' @param start Optional pooled starting values.
#' @param ... Additional `nlme` arguments.
#' @return A nonlinear `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("dose_response")
#' # Example 1: four-parameter logistic
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_dose_response(d, "response", 
#'       "dose", "unit")
#' }
#' # Example 2: subset of units
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) mixed_dose_response(subset(d, 
#'       as.integer(sub("U", "", unit)) <= 20), "response", "dose", "unit")
#' }
#' # Example 3: user-supplied pooled starts
#' \donttest{
#'   if (requireNamespace("nlme", quietly = TRUE)) {
#'       st <- stats::getInitial(response ~ SSfpl(dose, A, B, xmid, scal), d)
#'       mixed_dose_response(d, "response", "dose", "unit", st)
#'   }
#' }
mixed_dose_response <- function(data,response,dose,group,start=NULL,...) {
  .mf_need("nlme","dose-response nonlinear mixed models"); f<-stats::as.formula(paste(.mf_bt(response),"~ SSfpl(",.mf_bt(dose),", A, B, xmid, scal)")); if(is.null(start)) start<-stats::getInitial(f,data)
  fit<-.mf_fit(quote(nlme::nlme),f,data,formula_arg="model",fixed=A+B+xmid+scal~1,random=A~1,groups=stats::as.formula(paste("~",.mf_bt(group))),start=start,...)
  .mf_wrap(fit,"nlme",match.call(),data,specification=list(dose=dose,group=group))
}

#' Fit a random regression curve
#' @param response Response column.
#' @param time Quantitative trajectory variable.
#' @param data Data frame.
#' @param group Grouping factor.
#' @param degree Polynomial degree for fixed and random trajectory terms.
#' @param fixed Additional fixed-effect RHS text.
#' @param ... Additional `lme4::lmer` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("longitudinal")
#' # Example 1: linear random regression
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_curve("height", 
#'       "time", d, "subject", 1, "treatment")
#' }
#' # Example 2: quadratic trajectory
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_curve("height", 
#'       "time", d, "subject", 2, "treatment")
#' }
#' # Example 3: trajectory only
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) mixed_random_curve("height", 
#'       "time", d, "subject", 1)
#' }
mixed_random_curve <- function(response,time,data,group,degree=1,fixed="1",...) {
  .mf_need("lme4","random regression curves"); poly<-paste0("poly(",.mf_bt(time),",",degree,",raw=TRUE)"); f<-stats::as.formula(paste(.mf_bt(response),"~",fixed,"+",poly,"+ (",poly,"|",.mf_bt(group),")")); fit<-.mf_fit(quote(lme4::lmer),f,data,...); .mf_wrap(fit,"lme4",match.call(),data,specification=list(random_regression=TRUE,degree=degree))
}
