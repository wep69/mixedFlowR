#' Fit a P-spline spatial field-trial model
#' @param data Field-trial data.
#' @param response Response column.
#' @param genotype Genotype/treatment column.
#' @param col,row Numeric field coordinates.
#' @param fixed Optional fixed design term: a one-sided formula (`~ block`) or the name(s) of design columns.
#' @param random Optional random design term: a one-sided formula (`~ block`) or the name(s) of design columns.
#' @param genotype_as_random Treat genotype as random.
#' @param nseg Number of P-spline segments in each direction.
#' @param ... Additional `SpATS::SpATS` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' s<-mixed_data("spatial")
#' # Example 1: genotype fixed, blocks random
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) mixed_spatial_field(s, "yield", 
#'       "genotype", "col", "row", random = ~block)
#' }
#' # Example 2: genotype random
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) mixed_spatial_field(s, "yield", 
#'       "genotype", "col", "row", random = ~block, genotype_as_random = TRUE)
#' }
#' # Example 3: finer spatial basis
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) mixed_spatial_field(s, "yield", 
#'       "genotype", "col", "row", random = ~block, nseg = c(8, 10))
#' }
mixed_spatial_field <- function(data,response,genotype,col,row,fixed=NULL,random=NULL,genotype_as_random=FALSE,nseg=c(6,6),...) {
  .mf_need("SpATS","P-spline spatial field-trial models"); spatial<-stats::as.formula(paste("~ PSANOVA(",.mf_bt(col),",",.mf_bt(row),", nseg=c(",paste(nseg,collapse=","),"))"),env=asNamespace("SpATS"))
  fixed<-.mf_rhs_formula(fixed,data,"fixed"); random<-.mf_rhs_formula(random,data,"random")
  fit<-SpATS::SpATS(response=response,genotype=genotype,genotype.as.random=genotype_as_random,spatial=spatial,fixed=fixed,random=random,data=data,...)
  .mf_wrap(fit,"SpATS",match.call(),data,specification=list(coordinates=c(col,row),genotype=genotype,nseg=nseg))
}

#' Extract and plot the fitted SpATS spatial surface
#' @param object `SpATS` or wrapped field-trial fit.
#' @param grid Number of grid points in x and y directions.
#' @return A list containing grid data and a publication-ready heat map.
#' @export
#' @examples
#' s<-mixed_data("spatial")
#' # Example 1: default surface
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) {
#'       m <- mixed_spatial_field(s, "yield", "genotype", "col", "row", random = ~block)
#'       mixed_spatial_surface(m)
#'   }
#' }
#' # Example 2: compact grid
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) {
#'       m <- mixed_spatial_field(s, "yield", "genotype", "col", "row", random = ~block)
#'       mixed_spatial_surface(m, c(20, 20))
#'   }
#' }
#' # Example 3: genotype random
#' \donttest{
#'   if (requireNamespace("SpATS", quietly = TRUE)) {
#'       m <- mixed_spatial_field(s, "yield", "genotype", "col", "row", random = ~block, 
#'           genotype_as_random = TRUE)
#'       mixed_spatial_surface(m, c(25, 25))
#'   }
#' }
mixed_spatial_surface <- function(object,grid=c(50,50)) {
  .mf_need("SpATS","spatial-trend prediction"); m<-.mf_model(object); z<-SpATS::obtain.spatialtrend(m,grid=grid); d<-expand.grid(x=z$col.p,y=z$row.p); d$trend<-as.vector(t(z$fit))
  p<-ggplot2::ggplot(d,ggplot2::aes(x=.data$x,y=.data$y,fill=.data$trend))+ggplot2::geom_raster()+ggplot2::scale_fill_viridis_c()+ggplot2::coord_equal()+.mf_theme()+ggplot2::labs(x="Field x",y="Field y",fill="Spatial trend")
  list(data=d,plot=p,native=z)
}

#' Fit a multi-environment mixed model with sommer
#' @param fixed Fixed-effects formula.
#' @param random Random-effects formula.
#' @param data Multi-environment data.
#' @param rcov Residual covariance formula; defaults to `~units`.
#' @param solver `mmes` (recommended for many sparse mixed-model problems) or `mmer`.
#' @param ... Additional `sommer` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' m<-mixed_data("met")
#' # Example 1: genotype and environment random components
#' \donttest{if(requireNamespace("sommer",quietly=TRUE)) mixed_met(yield~1,~genotype+environment,m)}
#' # Example 2: environment fixed and genotype random
#' \donttest{if(requireNamespace("sommer",quietly=TRUE)) mixed_met(yield~environment,~genotype,m)}
#' # Example 3: legacy dense solver
#' \donttest{
#'   if (requireNamespace("sommer", quietly = TRUE)) mixed_met(yield ~ environment, 
#'       ~genotype, m, solver = "mmer")
#' }
mixed_met <- function(fixed,random,data,rcov=~units,solver=c("mmes","mmer"),...) {
  .mf_need("sommer","multi-environment mixed models"); solver<-match.arg(solver); fit<-.mf_with_attached(c("sommer","enhancer"), if(solver=="mmes") sommer::mmes(fixed=fixed,random=random,rcov=rcov,data=data,...) else sommer::mmer(fixed=fixed,random=random,rcov=rcov,data=data,...))
  .mf_wrap(fit,"sommer",match.call(),data,specification=list(solver=solver,random=random,rcov=rcov))
}

#' Fit genotype-by-environment covariance models
#' @param data Multi-environment data.
#' @param response Response column.
#' @param genotype Genotype column.
#' @param environment Environment column.
#' @param model `diagonal`, `unstructured`, or reduced-rank factor-analytic (`fa`).
#' @param nPC Number of retained factors for `fa`.
#' @param ... Additional `sommer::mmes` arguments.
#' @return A `mixedflow_fit`.
#' @export
#' @examples
#' d<-mixed_data("met")
#' # Example 1: diagonal GxE covariance
#' \donttest{
#'   if (requireNamespace("sommer", quietly = TRUE)) mixed_gxe(d, "yield", "genotype", 
#'       "environment", "diagonal")
#' }
#' # Example 2: unstructured GxE covariance
#' \donttest{
#'   if (requireNamespace("sommer", quietly = TRUE)) mixed_gxe(d, "yield", "genotype", 
#'       "environment", "unstructured")
#' }
#' # Example 3: two-factor reduced-rank model
#' \donttest{
#'   if (requireNamespace("sommer", quietly = TRUE)) mixed_gxe(d, "yield", "genotype", 
#'       "environment", "fa", nPC = 2)
#' }
mixed_gxe <- function(data,response,genotype,environment,model=c("fa","diagonal","unstructured"),nPC=2,...) {
  .mf_need("sommer","GxE covariance models"); model<-match.arg(model); fixed<-stats::as.formula(paste(.mf_bt(response),"~",.mf_bt(environment)))
  # sommer resolves the operators of its random formula through the search path and
  # through the calling frame, not through the formula environment. The structure
  # functions are therefore attached here and the reduced-rank input is bound as a
  # local object of this frame, which is where sommer looks for it.
  attached<-character(0)
  for(p in c("sommer","enhancer")) {
    if(requireNamespace(p,quietly=TRUE) && !paste0("package:",p) %in% search()) {
      suppressPackageStartupMessages(attachNamespace(asNamespace(p))); attached<-c(attached,p)
    }
  }
  on.exit(for(p in rev(attached)) try(detach(paste0("package:",p),character.only=TRUE,unload=FALSE),silent=TRUE),add=TRUE)
  ..mf_H<-NULL
  if(model=="fa") {
    # rrm() of the current sommer stack takes the grouping vector plus a two-way
    # table of identifiers by features (genotypes by environments). That table is
    # the observed genotype-by-environment means, and it must be complete: a
    # missing genotype-environment combination is a design gap, not a zero.
    ..mf_H<-tapply(data[[response]],list(data[[genotype]],data[[environment]]),mean,na.rm=TRUE)
    if(anyNA(..mf_H)) stop("The reduced-rank (factor-analytic) model needs every genotype observed in every environment. Combinations without data are design gaps; use model='diagonal' or complete the trial before requesting latent factors.",call.=FALSE)
    if(nPC>ncol(..mf_H)) stop(sprintf("nPC=%d exceeds the %d environments available; the number of latent factors cannot exceed the number of features.",nPC,ncol(..mf_H)),call.=FALSE)
  }
  rand_text<-switch(model,
    diagonal=paste0("~ vsm(dsm(",.mf_bt(environment),"), ism(",.mf_bt(genotype),"))"),
    unstructured=paste0("~ vsm(usm(",.mf_bt(environment),"), ism(",.mf_bt(genotype),"))"),
    fa=paste0("~ vsm(usm(rrm(",.mf_bt(environment),", ..mf_H, nPC=",nPC,")), ism(",.mf_bt(genotype),"))"))
  random<-stats::as.formula(rand_text); environment(random)<-environment()
  fit<-sommer::mmes(fixed=fixed,random=random,rcov=~units,data=data,...)
  .mf_wrap(fit,"sommer",match.call(),data,specification=list(gxe=model,nPC=nPC,genotype=genotype,environment=environment))
}
