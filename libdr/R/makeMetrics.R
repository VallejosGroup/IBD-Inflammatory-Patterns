#' Produce table of model fit metrics (AIC, BIC, Log-likelihood) for LCMMs
#' @param Gs Vector indicating which values of G should metrics be calculated
#'   for.
#' @param models List containing LCMM model fits
#' @export
makeMetrics <- function (Gs, models) {
  hlme.metrics <- matrix(c(numeric(), numeric(), numeric()), ncol = 3)
  for (G in Gs) {
    temp <- matrix(c(models[[G]]$loglik, models[[G]]$AIC, models[[G]]$BIC), ncol = 3)
    hlme.metrics <- rbind(hlme.metrics, temp)
  }
  rownames(hlme.metrics) <- Gs
  return(hlme.metrics)
}
