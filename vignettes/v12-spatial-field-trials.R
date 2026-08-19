## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, error = FALSE,
  collapse = TRUE, comment = "#>", fig.align = "center",
  fig.width = 7, fig.height = 5, dpi = 150
)
set.seed(260818)

## ----data---------------------------------------------------------------------
library(mixedFlowR)
s <- mixed_data("spatial")
head(s)

## ----dimensions---------------------------------------------------------------
range(s$row)
range(s$col)
length(unique(s$genotype))
with(s, table(block))

## ----raw-map, fig.height=5.5--------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(s, ggplot2::aes(col, row, fill = yield)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_equal() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      x = "Field column",
      y = "Field row",
      fill = "Yield",
      title = "Observed field response"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

## ----raw-genotype-------------------------------------------------------------
raw_gen <- aggregate(yield ~ genotype, s, mean)
raw_gen[order(-raw_gen$yield), ][1:8, ]

## ----spatial-exp--------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_exp <- mixed_spatial_covariance(
    data      = s,
    response  = "yield",
    fixed     = "genotype + block",
    x         = "col",
    y         = "row",
    structure = "exp"
  )

  m_exp
}

## ----spatial-gau--------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_gau <- mixed_spatial_covariance(
    s, "yield", "genotype + block", "col", "row",
    structure = "gau"
  )
}

## ----spatial-mat--------------------------------------------------------------
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  m_mat <- mixed_spatial_covariance(
    s, "yield", "genotype + block", "col", "row",
    structure = "mat"
  )
}

## ----spatial-compare----------------------------------------------------------
if (exists("m_exp") && exists("m_gau") && exists("m_mat")) {
  mixed_covariance_compare(
    Exponential = m_exp,
    Gaussian    = m_gau,
    Matern      = m_mat
  )
}

## ----exp-diagnose-------------------------------------------------------------
if (exists("m_exp")) {
  mixed_diagnose(m_exp)
  mixed_plot(m_exp, type = "residuals")
}

## ----spats-fit----------------------------------------------------------------
if (requireNamespace("SpATS", quietly = TRUE)) {
  spats_fit <- mixed_spatial_field(
    data     = s,
    response = "yield",
    genotype = "genotype",
    col      = "col",
    row      = "row",
    random   = ~ block,
    nseg     = c(6, 6)
  )

  spats_fit
}

## ----spatial-surface, fig.height=5.5------------------------------------------
if (exists("spats_fit")) {
  surface <- mixed_spatial_surface(
    spats_fit,
    grid = c(50, 50)
  )

  surface$plot
}

## ----spatial-wrapper, fig.height=5.5------------------------------------------
if (exists("spats_fit")) {
  mixed_plot(
    spats_fit,
    type = "spatial"
  )
}

## ----spats-random-genotype----------------------------------------------------
if (requireNamespace("SpATS", quietly = TRUE)) {
  spats_random <- mixed_spatial_field(
    s,
    response = "yield",
    genotype = "genotype",
    col = "col",
    row = "row",
    random = ~ block,
    genotype_as_random = TRUE,
    nseg = c(6, 6)
  )
}

## ----spats-finer--------------------------------------------------------------
if (requireNamespace("SpATS", quietly = TRUE)) {
  spats_fine <- mixed_spatial_field(
    s,
    "yield",
    "genotype",
    "col",
    "row",
    random = ~ block,
    nseg = c(8, 10)
  )
}

## ----export-spatial, eval=FALSE-----------------------------------------------
# if (exists("spats_fit")) {
#   mixed_plot(
#     spats_fit,
#     type   = "spatial",
#     file   = "field_spatial_trend.tiff",
#     width  = 7,
#     height = 5.5,
#     dpi    = 600
#   )
# }

