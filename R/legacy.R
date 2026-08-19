#' Fit a classical split-plot ANOVA
#'
#' Fits the legacy balanced split-plot representation with whole-plot and subplot error strata using `stats::aov()`.
#' @param data Data frame.
#' @param response,block,whole,sub Column names for response, block, whole-plot factor and subplot factor.
#' @return An `aovlist` object with class `mixedflow_legacy_splitplot` added.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: standard balanced split-plot
#' fit_legacy_splitplot(d,"yield","block","variety","nitrogen")
#' # Example 2: nitrogen treated as a factor
#' d$nitrogen_f <- factor(d$nitrogen)
#' fit_legacy_splitplot(d,"yield","block","variety","nitrogen_f")
#' # Example 3: shifted response, same randomization
#' d$yield2 <- d$yield + 10
#' fit_legacy_splitplot(d,"yield2","block","variety","nitrogen")
fit_legacy_splitplot <- function(data, response, block, whole, sub) {
  vars <- c(response,block,whole,sub); miss <- setdiff(vars,names(data)); if(length(miss)) stop("Missing variables: ",paste(miss,collapse=", "),call.=FALSE)
  f <- stats::as.formula(paste(.mf_bt(response),"~",.mf_bt(whole),"*",.mf_bt(sub),"+ Error(",.mf_bt(block),"/",.mf_bt(whole),")"))
  environment(f) <- parent.frame()
  fit <- stats::aov(f, data=data)
  class(fit) <- c("mixedflow_legacy_splitplot",class(fit)); attr(fit,"mixedflow_spec") <- list(response=response,block=block,whole=whole,sub=sub)
  fit
}

#' Expected mean-square map for a balanced split-plot
#'
#' Returns degrees of freedom, conceptual expected mean-square roles and the correct denominator for each treatment test.
#' @param data Data frame.
#' @param block,whole,sub Column names.
#' @return A pedagogical EMS table.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: standard EMS map
#' splitplot_ems(d,"block","variety","nitrogen")
#' # Example 2: factor-coded nitrogen
#' d$nf <- factor(d$nitrogen); splitplot_ems(d,"block","variety","nf")
#' # Example 3: four blocks subset
#' splitplot_ems(subset(d, block %in% levels(block)[1:4]),"block","variety","nitrogen")
splitplot_ems <- function(data, block, whole, sub) {
  b <- length(unique(data[[block]])); a <- length(unique(data[[whole]])); s <- length(unique(data[[sub]]))
  data.frame(
    source=c("Block","Whole plot factor","Whole-plot error","Subplot factor","Whole x Sub","Subplot error"),
    df=c(b-1,a-1,(b-1)*(a-1),s-1,(a-1)*(s-1),a*(b-1)*(s-1)),
    expected_mean_square=c("sigma_eB^2 + treatment-independent block terms","sigma_eB^2 + whole-plot treatment signal","sigma_eB^2 (block x whole-plot stratum)","sigma_e^2 + subplot treatment signal","sigma_e^2 + interaction signal","sigma_e^2"),
    denominator=c(NA,"Whole-plot error",NA,"Subplot error","Subplot error",NA),
    stringsAsFactors=FALSE)
}

#' Extract classical split-plot ANOVA strata
#' @param object Result from [fit_legacy_splitplot()].
#' @return A tidy data frame of ANOVA strata.
#' @export
#' @examples
#' d <- mixed_data("splitplot"); f <- fit_legacy_splitplot(d,"yield","block","variety","nitrogen")
#' # Example 1: standard extraction
#' splitplot_anova(f)
#' # Example 2: factor-coded nitrogen
#' d$nf <- factor(d$nitrogen)
#' splitplot_anova(fit_legacy_splitplot(d, "yield", "block", "variety", "nf"))
#' # Example 3: transformed response
#' d$logy <- log(d$yield)
#' splitplot_anova(fit_legacy_splitplot(d, "logy", "block", "variety", "nitrogen"))
splitplot_anova <- function(object) {
  if (!inherits(object,"aovlist")) stop("object must be an aovlist from a split-plot analysis.",call.=FALSE)
  .mf_flatten_aov(object)
}

#' Compare legacy split-plot and mixed-model representations
#'
#' Fits the classical error-stratum ANOVA and the corresponding Gaussian mixed model. This is intended for balanced designs and teaching/auditing.
#' @param data Data frame.
#' @param response,block,whole,sub Column names.
#' @param REML Use REML for the mixed model.
#' @return A list containing legacy and `lme4` fits plus fixed and variance-component summaries.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: canonical comparison
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) compare_legacy_mixed(d, "yield", 
#'       "block", "variety", "nitrogen")
#' }
#' # Example 2: nitrogen factor
#' d$nf <- factor(d$nitrogen)
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) compare_legacy_mixed(d, "yield", 
#'       "block", "variety", "nf")
#' }
#' # Example 3: ML sensitivity fit
#' \donttest{
#'   if (requireNamespace("lme4", quietly = TRUE)) compare_legacy_mixed(d, "yield", 
#'       "block", "variety", "nitrogen", REML = FALSE)
#' }
compare_legacy_mixed <- function(data,response,block,whole,sub,REML=TRUE) {
  .mf_need("lme4","legacy-to-mixed comparison")
  legacy <- fit_legacy_splitplot(data,response,block,whole,sub)
  f <- stats::as.formula(paste(.mf_bt(response),"~",.mf_bt(whole),"*",.mf_bt(sub),"+ (1|",.mf_bt(block),") + (1|",.mf_bt(block),":",.mf_bt(whole),")"))
  environment(f) <- asNamespace("lme4")
  mm <- .mf_fit(quote(lme4::lmer),f,data,REML=REML)
  list(legacy=legacy,mixed=.mf_wrap(mm,"lme4",data=data,specification=list(design="splitplot")),
       legacy_anova=splitplot_anova(legacy),fixed=.mf_tidy_fixed(mm),variance=.mf_tidy_varcorr(mm))
}

#' Fit a classical split-split-plot ANOVA
#' @param data Data frame.
#' @param response,block,whole,sub,subsub Column names for the three treatment strata.
#' @return An `aovlist` object.
#' @export
#' @examples
#' d <- mixed_data("splitplot"); d$stage <- factor(rep(c("S1","S2"), length.out=nrow(d)))
#' # Example 1: three treatment strata
#' fit_legacy_splitsplit(d,"yield","block","variety","nitrogen","stage")
#' # Example 2: factor-coded nitrogen
#' d$nf <- factor(d$nitrogen); fit_legacy_splitsplit(d,"yield","block","variety","nf","stage")
#' # Example 3: centered response
#' d$yc <- d$yield-mean(d$yield); fit_legacy_splitsplit(d,"yc","block","variety","nitrogen","stage")
fit_legacy_splitsplit <- function(data,response,block,whole,sub,subsub) {
  vars<-c(response,block,whole,sub,subsub); if(length(setdiff(vars,names(data)))) stop("One or more variables are missing.",call.=FALSE)
  f<-stats::as.formula(paste(.mf_bt(response),"~",.mf_bt(whole),"*",.mf_bt(sub),"*",.mf_bt(subsub),"+ Error(",.mf_bt(block),"/",.mf_bt(whole),"/",.mf_bt(sub),")"))
  stats::aov(f,data=data)
}

#' Fit a classical strip-plot ANOVA
#' @param data Data frame.
#' @param response,block,strip_a,strip_b Column names.
#' @return An `aovlist` object with separate block-by-strip error strata.
#' @export
#' @examples
#' d <- mixed_data("splitplot"); d$A <- factor(d$variety); d$B <- factor(d$nitrogen)
#' # Example 1: strip factors A and B
#' fit_legacy_stripplot(d,"yield","block","A","B")
#' # Example 2: subset of nitrogen levels
#' fit_legacy_stripplot(subset(d,nitrogen<=100),"yield","block","A","B")
#' # Example 3: centered response
#' d$yc<-d$yield-mean(d$yield); fit_legacy_stripplot(d,"yc","block","A","B")
fit_legacy_stripplot <- function(data,response,block,strip_a,strip_b) {
  vars<-c(response,block,strip_a,strip_b); if(length(setdiff(vars,names(data)))) stop("One or more variables are missing.",call.=FALSE)
  f<-stats::as.formula(paste(.mf_bt(response),"~",.mf_bt(strip_a),"*",.mf_bt(strip_b),"+ Error(",.mf_bt(block)," + ",.mf_bt(block),":",.mf_bt(strip_a)," + ",.mf_bt(block),":",.mf_bt(strip_b),")"))
  stats::aov(f,data=data)
}
