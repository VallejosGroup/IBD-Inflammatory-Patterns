library(ggplot2)
library(ggalluvial)
library(patchwork)

if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/cvallejo-predicct/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}

p1 <- readRDS(paste0(prefix, "processed/plots/fc-alluvial.RDS"))
p2 <- readRDS(paste0(prefix, "processed/plots/crp-alluvial.RDS"))

p <- p1 / p2  +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 13))

ggsave("plots/Figure-3.pdf",
       p,
       width = 8,
       height = 8,
       units = "in")
