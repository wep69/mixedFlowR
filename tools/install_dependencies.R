args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) normalizePath(args[[1]], mustWork=TRUE) else normalizePath(".", mustWork=TRUE)
d <- read.dcf(file.path(pkg,"DESCRIPTION"))[1,]
parse_field <- function(x) {
  x <- gsub("\\n", " ", x); x <- unlist(strsplit(x,",")); x <- trimws(x)
  x <- gsub("\\s*\\(.*\\)$", "", x); x[nzchar(x) & x != "R"]
}
imports <- parse_field(d[["Imports"]])
suggests <- parse_field(d[["Suggests"]])
core <- unique(c(imports,"roxygen2","testthat","rcmdcheck","pkgload","knitr","rmarkdown"))
install.packages(setdiff(core,rownames(installed.packages())), repos="https://cloud.r-project.org")
cat("Core development dependencies installed.\n")
cat("Optional scientific engines not yet installed:\n")
print(setdiff(suggests,rownames(installed.packages())))
cat("Install all optional engines only if needed with:\n")
cat("install.packages(c(",paste(sprintf('"%s"',suggests),collapse=","),"), repos='https://cloud.r-project.org')\n",sep="")
