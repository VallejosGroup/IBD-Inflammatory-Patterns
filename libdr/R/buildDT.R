#' Build DT::datatable objects from matrix of fit statistics
#' @param hlme.metrics A data frame or matrix of model fit metrics for different
#' number of assumed clusters (G)
#' @export
buildDT <- function(hlme.metrics) {
  hlme.metrics <- cbind(hlme.metrics,
    group = as.numeric(rownames(hlme.metrics))
  )
  hlme.metrics <- hlme.metrics[, c(4, 1, 2, 3)]
  knitr::kable(hlme.metrics,
    row.names = FALSE,
    col.names = c(
      "Clusters",
      "Maximum log-likelihood",
      "AIC",
      "BIC"
    )
  )
}
