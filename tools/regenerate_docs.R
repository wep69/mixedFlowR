args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) args[[1]] else "."
if (!requireNamespace("roxygen2", quietly = TRUE)) stop("Install roxygen2 >= 8.1.0 first.")
if (utils::packageVersion("roxygen2") < "8.1.0") stop("roxygen2 >= 8.1.0 is required for the documented release workflow.")
roxygen2::roxygenise(pkg, roclets = c("rd", "namespace"))
cat("Regenerated man/ and NAMESPACE from roxygen comments.\n")
