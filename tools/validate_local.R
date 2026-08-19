args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) normalizePath(args[[1]], mustWork=TRUE) else normalizePath(".", mustWork=TRUE)
options(repos=c(CRAN="https://cloud.r-project.org"))

need <- c("roxygen2","testthat","rcmdcheck")
miss <- need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
if(length(miss)) stop("Install development packages first: ",paste(miss,collapse=", "))

cat("1/5 Regenerating documentation and NAMESPACE...\n")
roxygen2::roxygenise(pkg,roclets=c("rd","namespace"))

cat("2/5 Running testthat suite...\n")
testthat::test_local(pkg,reporter="summary",stop_on_failure=TRUE)

cat("3/5 Checking 70-block registry and examples...\n")
suppressPackageStartupMessages(pkgload::load_all(pkg,quiet=TRUE))
stopifnot(nrow(mixed_blocks())==70L,all(mixed_blocks()$status=="implemented"))

cat("4/5 Running R CMD check --as-cran...\n")
chk <- rcmdcheck::rcmdcheck(pkg,args="--as-cran",error_on="never",check_dir=tempdir())
print(chk)
if(length(chk$errors)) stop("R CMD check returned errors.")

cat("5/5 Final status...\n")
if(length(chk$warnings)) warning("R CMD check returned warnings; review before release.")
if(length(chk$notes)) message("R CMD check returned notes; review before release.")
cat("Validation script completed. Runtime results must be archived with sessionInfo().\n")
print(sessionInfo())
