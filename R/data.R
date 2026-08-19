#' Load a mixedFlowR teaching dataset
#'
#' Reads one of the small, frozen simulated datasets shipped with the package.
#' These data are intended for examples, tests and teaching; they are not empirical evidence.
#'
#' @param name One of `splitplot`, `longitudinal`, `counts`, `binomial`, `beta`,
#'   `spatial`, `met`, `growth`, or `dose_response`.
#' @return A data frame.
#' @export
#' @examples
#' # Example 1: split-plot data
#' head(mixed_data("splitplot"))
#' # Example 2: repeated measures
#' head(mixed_data("longitudinal"))
#' # Example 3: spatial field trial
#' head(mixed_data("spatial"))
mixed_data <- function(name = c("splitplot", "longitudinal", "counts", "binomial", "beta", "spatial", "met", "growth", "dose_response")) {
  name <- match.arg(name)
  p <- system.file("extdata", paste0(name, ".csv"), package = "mixedFlowR")
  if (!nzchar(p)) stop("Teaching dataset was not found in the installed package.", call. = FALSE)
  utils::read.csv(p, stringsAsFactors = TRUE)
}
