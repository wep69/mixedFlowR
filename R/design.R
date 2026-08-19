#' Audit an agronomic mixed-model design
#'
#' Examines factor support, balance, replication and the declared experimental hierarchy before a model is fitted.
#' The audit is descriptive and design-aware; it does not infer randomization that was not supplied by the user.
#'
#' @param data Data frame.
#' @param response Response column name.
#' @param block Optional block column name.
#' @param whole Optional whole-plot treatment column name.
#' @param sub Optional subplot treatment column name.
#' @param subject Optional repeated-measures subject column name.
#' @param time Optional time column name.
#' @return A `mixedflow_audit` object with variable roles, cell counts, balance diagnostics and warnings.
#' @export
#' @examples
#' # Example 1: balanced split-plot
#' d <- mixed_data("splitplot")
#' mixed_design_audit(d, "yield", block="block", whole="variety", sub="nitrogen")
#' # Example 2: repeated measurements
#' z <- mixed_data("longitudinal")
#' mixed_design_audit(z, "height", subject="subject", time="time")
#' # Example 3: deliberately incomplete design
#' d2 <- d[-1, ]
#' mixed_design_audit(d2, "yield", block="block", whole="variety", sub="nitrogen")
mixed_design_audit <- function(data, response, block=NULL, whole=NULL, sub=NULL, subject=NULL, time=NULL) {
  stopifnot(is.data.frame(data), response %in% names(data))
  roles <- c(response=response, block=block, whole=whole, sub=sub, subject=subject, time=time)
  roles <- roles[!vapply(roles, is.null, logical(1))]
  missing_vars <- setdiff(unname(roles), names(data))
  if (length(missing_vars)) stop("Missing design variables: ", paste(missing_vars, collapse=", "), call.=FALSE)
  factors <- unique(c(block, whole, sub, subject, time)); factors <- factors[!is.na(factors) & nzchar(factors)]
  balance <- if (length(factors)) mixed_balance(data, factors) else data.frame()
  hierarchy <- mixed_hierarchy(data, block=block, whole=whole, sub=sub, subject=subject, time=time)
  pseudo <- mixed_pseudorep(data, response=response, cluster=subject %||% if (!is.null(sub) && !is.null(whole) && !is.null(block)) interaction(data[[block]], data[[whole]], data[[sub]], drop=TRUE) else NULL,
                            treatment=whole %||% sub, quiet=TRUE)
  warnings <- character()
  if (anyNA(data[[response]])) warnings <- c(warnings, "Response contains missing values.")
  if (nrow(balance) && any(balance$n == 0)) warnings <- c(warnings, "At least one declared design cell is empty.")
  if (!is.null(subject) && is.null(time)) warnings <- c(warnings, "Subject was declared without an explicit time/order variable.")
  structure(list(n=nrow(data), roles=roles, balance=balance, hierarchy=hierarchy,
                 pseudoreplication=pseudo, warnings=unique(warnings)), class="mixedflow_audit")
}

`%||%` <- function(x, y) if (is.null(x) || length(x)==0L) y else x

#' Represent the declared experimental hierarchy
#'
#' Creates an edge list linking block, whole plot, subplot, subject and repeated-measure levels.
#' @param data Data frame.
#' @param block,whole,sub,subject,time Optional column names.
#' @return A list containing hierarchy roles and parent-child edges.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: split-plot hierarchy
#' mixed_hierarchy(d, block="block", whole="variety", sub="nitrogen")
#' # Example 2: longitudinal hierarchy
#' mixed_hierarchy(mixed_data("longitudinal"), subject="subject", time="time")
#' # Example 3: blocks only
#' mixed_hierarchy(d, block="block")
mixed_hierarchy <- function(data, block=NULL, whole=NULL, sub=NULL, subject=NULL, time=NULL) {
  roles <- Filter(Negate(is.null), list(block=block, whole=whole, sub=sub, subject=subject, time=time))
  if (!length(roles)) return(list(roles=character(), edges=data.frame(parent=character(), child=character())))
  vars <- unlist(roles, use.names=TRUE)
  miss <- setdiff(vars, names(data)); if (length(miss)) stop("Unknown hierarchy variables: ", paste(miss, collapse=", "), call.=FALSE)
  ord <- c("block","whole","sub","subject","time"); present <- ord[ord %in% names(roles)]
  edges <- data.frame(parent=character(), child=character(), relation=character())
  if (length(present)>1L) for (i in seq_len(length(present)-1L)) {
    edges <- rbind(edges, data.frame(parent=roles[[present[i]]], child=roles[[present[i+1L]]], relation="declared nested/order"))
  }
  list(roles=vars, edges=edges)
}

#' Check balance and support of design cells
#'
#' Expands the Cartesian product of declared factors and reports the observed count in every cell.
#' @param data Data frame.
#' @param factors Character vector of factor columns.
#' @return A data frame with one row per possible cell and its count `n`.
#' @export
#' @examples
#' d <- mixed_data("splitplot")
#' # Example 1: full split-plot cell support
#' mixed_balance(d, c("block","variety","nitrogen"))
#' # Example 2: treatment by time support
#' mixed_balance(mixed_data("longitudinal"), c("treatment","time"))
#' # Example 3: reveal one missing cell
#' mixed_balance(d[-1,], c("block","variety","nitrogen"))
mixed_balance <- function(data, factors) {
  stopifnot(is.data.frame(data), length(factors)>=1L)
  miss <- setdiff(factors, names(data)); if (length(miss)) stop("Unknown factor columns: ", paste(miss, collapse=", "), call.=FALSE)
  lev <- lapply(data[factors], function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
  names(lev) <- factors
  grid <- do.call(expand.grid, c(lev, stringsAsFactors=FALSE))
  obs <- stats::aggregate(rep(1, nrow(data)), data[factors], length); names(obs)[ncol(obs)] <- "n"
  out <- merge(grid, obs, by=factors, all.x=TRUE, sort=FALSE); out$n[is.na(out$n)] <- 0L
  out
}

#' Screen for potential pseudoreplication
#'
#' Reports how many observations occur within each declared experimental cluster and treatment. It is a screening tool,
#' not an automatic proof of pseudoreplication, because independence depends on randomization and the scientific design.
#' @param data Data frame.
#' @param response Response column name.
#' @param cluster Cluster/experimental-unit column name or vector identifying clusters.
#' @param treatment Optional treatment column name.
#' @param quiet Suppress warnings when `TRUE`.
#' @return A list with replication counts and screening flags.
#' @export
#' @examples
#' z <- mixed_data("longitudinal")
#' # Example 1: repeated observations within plant
#' mixed_pseudorep(z, "height", cluster="subject", treatment="treatment")
#' # Example 2: split-plot whole-plot clusters
#' d <- mixed_data("splitplot"); d$wp <- interaction(d$block,d$variety)
#' mixed_pseudorep(d, "yield", cluster="wp", treatment="variety")
#' # Example 3: no explicit cluster supplied
#' mixed_pseudorep(d, "yield", treatment="variety")
mixed_pseudorep <- function(data, response, cluster=NULL, treatment=NULL, quiet=FALSE) {
  if (!response %in% names(data)) stop("Response not found.", call.=FALSE)
  if (is.null(cluster)) {
    if (!quiet) warning("No experimental-unit/cluster variable was supplied; independence cannot be audited.")
    return(list(cluster_declared=FALSE, counts=data.frame(), potential_issue=NA))
  }
  cl <- if (length(cluster)==1L && is.character(cluster)) data[[cluster]] else cluster
  if (length(cl)!=nrow(data)) stop("cluster must identify every row.", call.=FALSE)
  tr <- if (!is.null(treatment)) data[[treatment]] else factor(rep("all", nrow(data)))
  tab <- as.data.frame(table(cluster=cl, treatment=tr), stringsAsFactors=FALSE); names(tab)[3] <- "n"
  tab <- tab[tab$n>0,]
  issue <- any(tab$n>1)
  list(cluster_declared=TRUE, counts=tab, repeated_within_cluster=issue,
       note="Repeated observations within a cluster require an analysis that preserves that dependence; repetition alone does not prove pseudoreplication.")
}
