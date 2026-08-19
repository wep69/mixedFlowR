## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = "center",
                      fig.width = 7, fig.height = 5)
library(mixedFlowR)
set.seed(260818)

## ----orientation--------------------------------------------------------------
mixed_capabilities()

## ----roadmap------------------------------------------------------------------
utils::head(mixed_blocks(), 12)

## ----teaching-data------------------------------------------------------------
d <- mixed_data("splitplot")
str(d)

