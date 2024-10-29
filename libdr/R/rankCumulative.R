#' Rank clusters by the area under their mean trajectories
#' @param model An \code{\link[lcmm]{hlme}} object
#' @inheritParams spaghettiPlot
#' @return A data frame with columns for the original cluster number (Original), the new rank (New), and the area calculated via the trapezoidal rule (Area).
#' @export
rankCumulative <- function(model, tmax = 7, var.time = "calpro_time") {
  Original <- New <- Area <- NULL
  time.pred <- seq(0, tmax, 0.01)
  if (var.time == "calpro_time") {
    pred.fc.df <- data.frame(
      calpro_time = c(time.pred, time.pred),
      diagnosis = c(
        rep("Crohn's Disease", length(time.pred)),
        rep("Ulcerative Colitis", length(time.pred))
      )
    )
    pred.fc.df.update <- lcmm::predictY(model,
      pred.fc.df,
      var.time = "calpro_time",
      draws = TRUE
    )$pred

    area <- c()

    for (g in 1:model$ng) {
      area <- c(
        area,
        pracma::trapz(
          x = pred.fc.df[1:length(time.pred), "calpro_time"],
          y = pred.fc.df.update[1:length(time.pred), g]
        )
      )
    }
  } else if (var.time == "crp_time") {
    pred.crp.df <- data.frame(
      crp_time = c(time.pred, time.pred),
      diagnosis = c(
        rep("Crohn's Disease", length(time.pred)),
        rep("Ulcerative Colitis", length(time.pred))
      )
    )

    pred.crp.df.update <- lcmm::predictY(model,
      pred.crp.df,
      var.time = "crp_time",
      draws = TRUE
    )$pred

    area <- c()

    for (g in 1:model$ng) {
      area <- c(
        area,
        pracma::trapz(
          x = pred.crp.df[1:length(time.pred), "crp_time"],
          y = pred.crp.df.update[1:length(time.pred), g]
        )
      )
    }
  } else {
    stop("Invalid time variable.")
  }

  output <- data.frame(Original = 1:model$ng, Area = area)
  output <- output[order(output$Area), ]
  output$New <- 1:nrow(output)
  output <- select(output, Original, New, Area)
  rownames(output) <- NULL
  return(output)
}
