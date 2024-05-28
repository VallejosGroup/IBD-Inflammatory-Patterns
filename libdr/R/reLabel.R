#' relabel alluvial class labels ensuring consistency.
#' @description Relabel class labels in an alluvial data frame to ensure class
#'   labels are consistent with the class labels used models assuming fewer
#'   classes
#' @param new.G The number of classes assumed by the LCMM which is now subject
#'   to class relabelling.
#' @param alluvial.df Data frame containing class assignments for subjects for
#'   differing G
#' @export
reLabel <- function(new.G, alluvial.df){
  # Global vars
  G <- ids <- NULL

  # labels for G - 1
  old.g <- subset(alluvial.df, G == new.G - 1)
  # labels for G (before being switched)
  new.g <- subset(alluvial.df, G == new.G)
  # Dictionary to hold old and new labels for G.
  dict <- data.frame(old.g = numeric(), new.g = numeric())

  potential <-  1:new.G # potential labels

  gs <- as.numeric(names(sort(table(new.g$class)[table(new.g$class) != 0],
                              decreasing = TRUE))) # Order new.g by size
  for (g in gs) {
    # Find old classes
    tab <- table(subset(old.g, ids %in% subset(new.g, class == g)$ids)$class)
    tab <- tab[tab != 0]

    new.g.cands <- as.numeric(names(sort(tab, decreasing = TRUE)))
    # Remove labels already assigned
    new.g.cands <- new.g.cands[new.g.cands %in% potential]

    if (length(potential) == 1) {
      # Smallest cluster, assign new cluster number
      dict <- rbind(dict, data.frame(old.g = g, new.g = new.G))
    } else {
      dict <- rbind(dict, data.frame(old.g = g, new.g = new.g.cands[1]))
    }
    # Stop label being assigned twice by removing it from list of potential labs
    potential <- potential[potential!=new.g.cands[1]]
  }

  alluvial.df[alluvial.df[, "G"] == new.G, "class" ] <-
    plyr::mapvalues(alluvial.df[alluvial.df[, "G"] == new.G, "class"],
                    from = dict[, "old.g"],
                    to = dict[, "new.g"])
  return(alluvial.df)
}
