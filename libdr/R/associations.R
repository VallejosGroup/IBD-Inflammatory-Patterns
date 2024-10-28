#' Percentage bar plots for categorical data
#' @param dat Data frame holding categorical and cluster assignment data.
#' @param var Character. The name of the categorical variable of interest.
#' @param class Character. The name of the class assignment variable. Assumed
#'   to be \code{"class_combined"} if not manually specified
#' @returns A \code{\link[patchwork]{patchwork}} object
#' @export
plotCat <- function(dat, var, class = "class_combined") {
  cluster <- NULL

  fill.vec <- c("#C2F8CB", "#EFC7E5", "#F6BD60", "blue")
  col.vec <- c("#82C68F", "#C795BB", "#C89216", "blue")

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
    patchwork::plot_layout(nrow = length(labels), ncol = 1, guides = "collect") &
    #patchwork::plot_annotation(tag_levels = "A") &
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  return(p)
}

.getCI <- function(m) {

  CI_factor <- qnorm(0.975)

  # A chatGPT edited version of the original code :-)
  m_summary <- summary(m)
  m_coeff <- as.data.frame(m_summary$coefficients) %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "Estimate") %>%
    mutate(var = paste(Var1, Var2))
  m_se <- as.data.frame(m_summary$standard.errors)  %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "SE") %>%
    mutate(var = paste(Var1, Var2)) %>%
    select(c(var, SE))
  m_coeff <- merge(m_coeff, m_se, by = "var") %>%
    mutate(z = Estimate/SE,
           p.val = (1 - pnorm(abs(z), 0, 1)) * 2,
           Lower = Estimate - CI_factor * SE,
           Upper = Estimate + CI_factor * SE,
           Sig = ifelse(Upper < 0 | Lower > 0, TRUE, FALSE)) %>%
    arrange(Var2, Var1) %>%
    subset(Var2 != "(Intercept)")

  return(m_coeff)
}

.plotCI <- function(tab, variable) {

  tab %>%
    filter(Var2 == variable) %>%
    ggplot(aes(
      x = Var1,
      y = Estimate,
      ymin = Lower,
      ymax = Upper,
      color = ifelse(Sig == TRUE, "red", "black"))) +
    geom_errorbar() +
    geom_point(size = 3.5) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() + # flip coordinates (puts labels on y axis)
    xlab("") +
    ylab("Estimate (95% CI)") +
    theme_bw() +
    scale_color_manual(values = c("black", "#FF007F")) +
    theme(legend.position = "none") +
    ggtitle(variable)
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

  # Multivariate analysis
  ## Fit the model
  mlr_multi <- nnet::multinom(formula = reformulate(var, class), data = dat, trace = FALSE)
  ## Extract coeffs and CIs
  tab_multi <- .getCI(mlr_multi)
  ## Plot
  p_multi <- list()
  for (variable in unique(tab_multi$Var2)) {
    p_multi[[variable]] <- .plotCI(tab_multi, variable)
  }

  # Univariate analysis
  if(length(var) > 1) {

    ## Fit the model + Extract coeffs and CIs
    tab_uni <- NULL
    for (variable in var) {
      mlr_uni <- nnet::multinom(formula = reformulate(variable, class),
                                data = dat, trace = FALSE)
      tab_uni <- rbind(tab_uni, .getCI(mlr_uni))
    }
    ## Plot
    p_uni <- list()
    for (variable in unique(tab_uni$Var2)) {
      p_uni[[variable]] <- .plotCI(tab_uni, variable)
    }
  }

  # Output
  if(length(var) == 1) {
    out <- list(tab_multi = tab_multi,
                plot_multi = p_multi)
  } else {
    out <- list(tab_multi = tab_multi,
                plot_multi = p_multi,
                tab_uni = tab_uni,
                plot_uni = p_uni)
  }
  return(out)
}
