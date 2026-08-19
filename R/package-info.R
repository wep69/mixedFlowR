#' Inspect mixedFlowR backend capabilities
#' @return A data frame showing whether each optional modelling engine is installed.
#' @export
#' @examples
#' # Example 1: all capabilities
#' mixed_capabilities()
#' # Example 2: inspect installed engines
#' subset(mixed_capabilities(), installed)
#' # Example 3: inspect unavailable engines
#' subset(mixed_capabilities(), !installed)
mixed_capabilities <- function() {
  pkgs<-c("lme4","nlme","glmmTMB","robustlmm","confintROB","clubSandwich","lmerTest","pbkrtest","emmeans","gamlss","gamlss.dist","brms","posterior","loo","DHARMa","performance","influence.ME","SpATS","sommer")
  data.frame(package=pkgs,installed=vapply(pkgs,requireNamespace,logical(1),quietly=TRUE),version=vapply(pkgs,function(p) if(requireNamespace(p,quietly=TRUE)) as.character(utils::packageVersion(p)) else NA_character_,character(1)),row.names=NULL)
}

#' Inspect the 70-block implementation registry
#' @param module Optional regular-expression filter for module names.
#' @return A data frame with block, module, public function and implementation status.
#' @export
#' @examples
#' # Example 1: all 70 blocks
#' mixed_blocks()
#' # Example 2: bootstrap blocks
#' mixed_blocks("Bootstrap")
#' # Example 3: spatial blocks
#' mixed_blocks("Spatial|field")
mixed_blocks <- function(module=NULL) {
  z<-do.call(rbind,lapply(seq_along(.mixedflow_blocks),function(i) data.frame(block=i,module=.mixedflow_blocks[[i]]$module,function_name=.mixedflow_blocks[[i]]$function_name,status=.mixedflow_blocks[[i]]$status,stringsAsFactors=FALSE)))
  if(!is.null(module)) z<-z[grepl(module,z$module,ignore.case=TRUE),,drop=FALSE]; rownames(z)<-NULL; z
}
