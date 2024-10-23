#' Percentage bar plots for categorical data
#' @param dat Data frame holding categorical and cluster assignment data.
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
        labs(x = "Cluster", y = "Percentage") +
        ggtitle(labels[i])
    } else {
      p <- p +
        perc.table %>%
        filter(label == labels[i]) %>%
        ggplot(aes(x = cluster, y = perc)) +
        geom_bar(stat = "identity", fill = fill.vec[i], color = col.vec[i]) +
        geom_hline(yintercept = totalPerc, linetype = "dashed", color = "#4D4730") +
        theme_minimal() +
        labs(x = "Cluster", y = "Percentage") +
        ggtitle(labels[i]) #+
        #theme(axis.title.x = element_blank(),
        #      axis.text.x = element_blank(),
        #      axis.ticks.x = element_blank())
    }
  }
  p <- p +
    patchwork::plot_layout(nrow = length(labels), ncol = 1, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A") &
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  return(p)
}


#' Forest plot of multinomial logistic regression model
#' @param dat  Data frame holding covariate and cluster assignment data.
#' @param var Character. The name of the covariate(s) of interest. If given as
#'   a character vector then a multivariate model is fitted using the specified
#'   covariates.
#' @param class Character. The name of the class assignment variable. Assumed
#'   to be \code{"class_combined"} if not manually specified.
#' @export
mlrPlot <- function(dat, var, class = "class_combined") {

  Estimate <- Lower <- Upper <- Var1 <- Var2 <- sig <- NULL

  results <- list()
  mlr <- nnet::multinom(formula = reformulate(var, class), data = dat, trace = FALSE)
  z <- summary(mlr)$coefficients / summary(mlr)$standard.errors
  p.val <- (1 - pnorm(abs(z), 0, 1)) * 2

  coeff <- summary(mlr)$coefficients
  coeff <- reshape2::melt(coeff)
  coeff$var <- with(coeff, paste(Var1, Var2))
  coeff <- filter(coeff, Var2 != "(Intercept)")
  coeff <- coeff
  colnames(coeff) <- c("Var1", "Var2", "Estimate", "var")
  lower <- summary(mlr)$coefficients - (qnorm(0.975) * summary(mlr)$standard.errors)
  lower <- reshape2::melt(lower)
  lower$var <- with(lower, paste(Var1, Var2))
  lower <- filter(lower, Var2 != "(Intercept)")
  lower <- lower %>%
    select(-Var1, -Var2)
  colnames(lower) <- c("Lower", "var")
  tab <- merge(coeff, lower, by = "var")
  upper <- summary(mlr)$coefficients + (qnorm(0.975) * summary(mlr)$standard.errors)
  upper <- reshape2::melt(upper)
  upper$var <- with(upper, paste(Var1, Var2))
  upper <- filter(upper, Var2 != "(Intercept)")
  upper <- upper %>%
    select(-Var1, -Var2)
  colnames(upper) <- c("Upper", "var")
  tab <- merge(tab, upper, by = "var")
  tab$var <- factor(tab$var, levels = rev(tab$var))
  tab$sig <- FALSE
  tab[tab$Upper < 0, "sig"] <- TRUE
  tab[tab$Lower > 0, "sig"] <- TRUE
  for (variable in unique(tab$Var2)) {
    p <- tab %>%
      filter(Var2 == variable) %>%
      ggplot(aes(
        x = var,
        y = Estimate,
        ymin = Lower,
        ymax = Upper,
        color = sig)) +
      geom_errorbar() +
      geom_point(size = 3.5) +
      geom_hline(yintercept = 0, lty = 2) +
      coord_flip() + # flip coordinates (puts labels on y axis)
      xlab("") +
      ylab("Estimate (95% CI)") +
      theme_bw() +
      scale_color_manual(values = c("black", "#FF007F")) +
      theme(legend.position = "none")
    print(p)
  }
  return(p.val)
}
