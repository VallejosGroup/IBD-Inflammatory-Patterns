#' Percentage bar plots for categorical data
#' @param dat data frame holding categorical data and cluster assignment data.
#' @param var Character. The name of the categorical variable of interest.
#' @param class Character. The name of the class assignment variable. Assumed
#'   to be \code{"class_combined"} if not manually specified
#' @returns A \code{\link[patchwork]{patchwork}} object
#' @export
plotCat <- function(dat, var, class = "class_combined") {
  cluster <- NULL

  fill.vec <- c("#C2F8CB", "#EFC7E5", "#F6BD60")
  col.vec <- c("#82C68F", "#C795BB", "#C89216")

  if (!(var %in% colnames(dat))) stop("Column for categorical variable not found.")
  if (!(class %in% colnames(dat))) stop("Class column not found.")

  labels <- sort(unique(dat[, var]))

  # Create df to hold percentages
  perc.table <- data.frame(
    label = character(),
    cluster = character(),
    perc = numeric()
  )

  # Calculate percentage per cluster for each category
  for (g in sort(unique(dat[, class]))) {
    temp.1 <- dat[dat[, class] == g, ]
    for (label in labels) {
      perc <- nrow(subset(temp.1, eval(parse(text = var)) == label)) / nrow(temp.1)
      perc.table <- rbind(
        perc.table,
        data.frame(
          label = label,
          cluster = g,
          perc = perc
        )
      )
    }
  }

  # Set order of clusters in percentage data frame
  perc.table$cluster <- factor(perc.table$cluster,
                               levels = levels(dat[, class]))
  # Per sub plot
  for (i in 1:length(labels)){
    totalPerc <- nrow(subset(dat, eval(parse(text = var)) == labels[i])) /
      nrow(dat)

    if (i == 1) {

      p <- perc.table %>%
        filter(label == labels[i]) %>%
        ggplot(aes(x = cluster, y = perc)) +
        geom_bar(stat = "identity", fill = fill.vec[i], color = col.vec[i]) +
        geom_hline(yintercept = totalPerc, linetype = "dashed", color = "#4D4730") +
        theme_minimal() +
        labs(x = "Cluster", y = "Percentage")
    } else {
      p <- p +
        perc.table %>%
        filter(label == labels[i]) %>%
        ggplot(aes(x = cluster, y = perc)) +
        geom_bar(stat = "identity", fill = fill.vec[i], color = col.vec[i]) +
        geom_hline(yintercept = totalPerc, linetype = "dashed", color = "#4D4730") +
        theme_minimal() +
        labs(x = "Cluster", y = "Percentage")
    }
  }
  p <- p +
    patchwork::plot_layout(nrow = length(labels), ncol = 1) +
    patchwork::plot_annotation(tag_levels = "A") &
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  return(p)
}
