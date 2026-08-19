# Internal utilities -------------------------------------------------------
.mf_need <- function(pkg, feature = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    what <- if (is.null(feature)) "this feature" else feature
    stop(sprintf("Package '%s' is required for %s. Install it and retry.", pkg, what), call. = FALSE)
  }
  invisible(TRUE)
}

.mf_need_version <- function(pkg, min_version, feature = NULL) {
  .mf_need(pkg, feature)
  if (utils::packageVersion(pkg) < min_version) {
    what <- if (is.null(feature)) "this feature" else feature
    stop(sprintf("Package '%s' >= %s is required for %s; installed version is %s.", pkg, min_version, what, as.character(utils::packageVersion(pkg))), call. = FALSE)
  }
  invisible(TRUE)
}

.mf_model <- function(x) {
  if (inherits(x, "mixedflow_fit")) x$model else x
}

# Fitting a model inside a function normally records a call whose 'data' argument
# is the internal variable name. Backends that RE-EVALUATE that call later then
# fail: lmerTest and pbkrtest cannot rebuild the deviance function, PBmodcomp
# finds utils::data instead of the experiment, and emmeans cannot recover the
# data. The fix is to bind the data under a stable name in an environment that
# travels with the formula, so the recorded call stays small and re-evaluable.
.mf_fit <- function(fun, formula, data, ..., formula_arg = "formula", data_arg = "data") {
  env <- new.env(parent = environment(formula))
  env$..mf_data <- data
  environment(formula) <- env
  args <- list(...)
  cl <- as.call(c(list(fun), stats::setNames(list(formula, quote(..mf_data)),
                                             c(formula_arg, data_arg)), args))
  eval(cl, envir = env)
}

# A few legacy consumers (influence.ME) do not evaluate the model call at all:
# they take the third element of the call as a character string and look the data
# up with get(). influence.ME reserves one name for that situation, "data.update",
# in which case it reads the model frame instead. Renaming the data argument to
# that reserved name therefore uses the backend's own supported path, without
# binding anything in the global environment.
.mf_influence_ready <- function(model) {
  cl <- tryCatch(methods::slot(model, "call"), error = function(e) NULL)
  if (is.null(cl) || length(cl) < 3L) return(model)
  if (!identical(as.character(cl)[3], "data.update")) {
    cl[["data"]] <- quote(data.update)
    methods::slot(model, "call") <- cl
  }
  model
}

# Some estimation backends resolve their own formula operators through the
# search path rather than through the formula environment: sommer needs vsm(),
# dsm(), ism() and rrm() (the last one supplied by its dependency 'enhancer'),
# and influence.ME expects lme4 to be attached. Attaching the namespace for the
# duration of the call keeps that requirement explicit and local.
.mf_with_attached <- function(pkgs, expr) {
  to_detach <- character(0)
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) next
    if (!paste0("package:", p) %in% search()) {
      suppressPackageStartupMessages(attachNamespace(asNamespace(p)))
      to_detach <- c(to_detach, p)
    }
  }
  on.exit({
    for (p in rev(to_detach)) {
      try(detach(paste0("package:", p), character.only = TRUE, unload = FALSE), silent = TRUE)
    }
  }, add = TRUE)
  force(expr)
}

.mf_engine <- function(x) {
  if (inherits(x, "mixedflow_fit")) x$engine else {
    z <- class(x)
    if (any(grepl("merMod", z))) "lme4" else if ("lme" %in% z || "nlme" %in% z) "nlme" else
      if ("glmmTMB" %in% z) "glmmTMB" else if ("rlmerMod" %in% z) "robustlmm" else
        if ("gamlss" %in% z) "gamlss" else if ("brmsfit" %in% z) "brms" else if ("SpATS" %in% z) "SpATS" else
          if (any(c("mmer", "mmes") %in% z)) "sommer" else "unknown"
  }
}

.mf_wrap <- function(model, engine, call = NULL, data = NULL, design = NULL, specification = list(), warnings = character()) {
  structure(list(call = call, engine = engine, model = model, data = data, design = design,
                 specification = specification, warnings = warnings,
                 audit = list(timestamp = as.character(Sys.time()), engine = engine)),
            class = "mixedflow_fit")
}

.mf_formula_text <- function(x) paste(deparse(x), collapse = " ")

.mf_response_name <- function(object) {
  m <- .mf_model(object)
  f <- tryCatch(stats::formula(m), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  lhs <- tryCatch(f[[2]], error = function(e) NULL)
  if (is.null(lhs)) return(NULL)
  vars <- all.vars(lhs)
  if (length(vars)) vars[[1]] else NULL
}
.mf_has_random_bars <- function(formula) grepl("\\|", .mf_formula_text(formula))
.mf_bt <- function(x) paste0("`", gsub("`", "", x), "`")

.mf_family <- function(family) {
  if (inherits(family, "family")) return(family)
  if (is.character(family) && length(family) == 1L) {
    if (family == "gaussian") return(stats::gaussian())
    if (family == "binomial") return(stats::binomial())
    if (family == "poisson") return(stats::poisson())
    if (family == "Gamma") return(stats::Gamma())
  }
  family
}

.mf_fixed_formula <- function(response, fixed) {
  if (inherits(fixed, "formula")) return(fixed)
  stats::as.formula(paste(.mf_bt(response), "~", fixed))
}

# Design terms that carry no response (blocks, replicates, spatial nuisance
# effects) are accepted either as a one-sided formula or as the names of the
# design columns, which is the string-first idiom used elsewhere in the package.
.mf_rhs_formula <- function(x, data = NULL, arg = "fixed") {
  if (is.null(x)) return(NULL)
  if (inherits(x, "formula")) {
    if (length(x) != 2L) {
      stop(sprintf("'%s' describes a design term without a response; supply a one-sided formula such as ~ block.", arg),
           call. = FALSE)
    }
    return(x)
  }
  if (is.character(x) && length(x) >= 1L && all(nzchar(x))) {
    if (!is.null(data)) {
      absent <- setdiff(x, names(data))
      if (length(absent)) {
        stop(sprintf("'%s' names design columns that are absent from the data: %s. A design term must exist in the experiment before it can be modelled.",
                     arg, paste(absent, collapse = ", ")), call. = FALSE)
      }
    }
    return(stats::as.formula(paste("~", paste(vapply(x, .mf_bt, character(1)), collapse = " + "))))
  }
  stop(sprintf("'%s' must be a one-sided formula (for example ~ block) or the name(s) of design columns.", arg),
       call. = FALSE)
}

.mf_random_formula <- function(terms, group, covariance = NULL) {
  left <- if (length(terms) == 0L) "1" else paste(terms, collapse = " + ")
  rhs <- paste0("(", left, " | ", .mf_bt(group), ")")
  if (!is.null(covariance)) rhs <- paste0(covariance, "(", left, " | ", .mf_bt(group), ")")
  rhs
}

.mf_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title.position = "plot",
                   legend.position = "right")
}

.mf_tidy_fixed <- function(model) {
  eng <- .mf_engine(model)
  m <- .mf_model(model)
  if (eng == "lme4") {
    est <- lme4::fixef(m); vc <- as.matrix(stats::vcov(m)); se <- sqrt(diag(vc))
    return(data.frame(term = names(est), estimate = unname(est), std.error = unname(se), row.names = NULL))
  }
  if (eng == "nlme") {
    est <- nlme::fixef(m); vc <- as.matrix(stats::vcov(m)); se <- sqrt(diag(vc))
    return(data.frame(term = names(est), estimate = unname(est), std.error = unname(se), row.names = NULL))
  }
  if (eng == "glmmTMB") {
    ss <- summary(m)$coefficients$cond
    return(data.frame(term = rownames(ss), estimate = ss[,1], std.error = ss[,2], statistic = ss[,3], p.value = ss[,4], row.names = NULL))
  }
  if (eng == "robustlmm") {
    est <- lme4::fixef(m); vc <- as.matrix(stats::vcov(m)); se <- sqrt(diag(vc))
    return(data.frame(term = names(est), estimate = unname(est), std.error = unname(se), row.names = NULL))
  }
  if (eng == "gamlss") {
    est <- stats::coef(m, what = "mu")
    return(data.frame(term = names(est), estimate = unname(est), row.names = NULL))
  }
  if (eng == "brms") {
    s <- as.data.frame(brms::fixef(m, summary = TRUE)); s$term <- rownames(s); rownames(s) <- NULL
    return(s[, c("term", setdiff(names(s), "term")), drop = FALSE])
  }
  stop("Fixed-effect extraction is unavailable for this engine.", call. = FALSE)
}

.mf_tidy_varcorr <- function(model) {
  eng <- .mf_engine(model); m <- .mf_model(model)
  if (eng %in% c("lme4", "robustlmm")) return(as.data.frame(lme4::VarCorr(m)))
  if (eng == "nlme") {
    x <- nlme::VarCorr(m); return(data.frame(component = rownames(x), x, row.names = NULL, check.names = FALSE))
  }
  if (eng == "glmmTMB") return(as.data.frame(glmmTMB::VarCorr(m)$cond))
  if (eng == "sommer") return(as.data.frame(summary(m)$varcomp))
  data.frame()
}

.mf_flatten_aov <- function(x) {
  s <- summary(x); out <- list(); k <- 0L
  for (stratum in names(s)) {
    tabs <- s[[stratum]]
    for (tab in tabs) {
      if (!is.data.frame(tab) && !is.matrix(tab)) next
      z <- as.data.frame(tab); z$term <- rownames(z); z$stratum <- stratum; rownames(z) <- NULL
      k <- k + 1L; out[[k]] <- z
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

.mf_varcorr_matrix <- function(model, group = NULL) {
  eng <- .mf_engine(model); m <- .mf_model(model)
  if (eng %in% c("lme4", "robustlmm")) {
    vc <- lme4::VarCorr(m); if (is.null(group)) group <- names(vc)[1]; return(as.matrix(vc[[group]]))
  }
  if (eng == "glmmTMB") {
    vc <- glmmTMB::VarCorr(m)$cond; if (is.null(group)) group <- names(vc)[1]; return(as.matrix(vc[[group]]))
  }
  stop("Covariance-matrix extraction is currently available for lme4, robustlmm and glmmTMB fits.", call. = FALSE)
}

.mf_formula_with_struct <- function(response, fixed, time, group, structure, engine) {
  base <- if (inherits(fixed, "formula")) paste(deparse(fixed[[3]]), collapse=" ") else fixed
  lhs <- .mf_bt(response)
  tf <- if (identical(engine, "lme4") && identical(structure, "ar1")) {
    paste0("ordered(", .mf_bt(time), ")")
  } else {
    paste0("factor(", .mf_bt(time), ")")
  }
  term <- switch(structure,
    ar1 = paste0("ar1(0 + ", tf, " | ", .mf_bt(group), ")"),
    cs = paste0("cs(0 + ", tf, " | ", .mf_bt(group), ")"),
    diag = paste0("diag(0 + ", tf, " | ", .mf_bt(group), ")"),
    us = paste0("us(0 + ", tf, " | ", .mf_bt(group), ")"),
    toep = paste0("toep(0 + ", tf, " | ", .mf_bt(group), ")"),
    stop("Unsupported structured covariance term.", call. = FALSE))
  f <- stats::as.formula(paste(lhs, "~", base, "+", term))
  if (engine == "lme4") {
    .mf_need_version("lme4", "2.0-1", "structured covariance terms")
    environment(f) <- asNamespace("lme4")
  }
  if (engine == "glmmTMB") environment(f) <- asNamespace("glmmTMB")
  f
}
