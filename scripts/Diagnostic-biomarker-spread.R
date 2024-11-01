library(tidyverse)

if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/Vallejo-predict/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}


fc.dist <- readRDS(paste0(prefix, "processed/fc-diag-dist.RDS"))
model.fcal <- readRDS(paste0(prefix, "/cache/fcal/ncs/fcal-8.RDS"))

fc.dist <- subset(fc.dist, ids %in% model.fcal$pprob$ids)
fc.dist <- merge(fc.dist, model.fcal$pprob[, c("ids", "class")], by = "ids")
fc.dist$class <- paste0("FC", fc.dist$class)
fc.dist$class <- plyr::mapvalues(fc.dist$class,
                                 from = paste0("FC", seq(1, 8)),
                                 to = paste0("FC", c(7, 6, 4, 8, 1, 5, 2, 3)))

p <- fc.dist %>%
  ggplot(aes(x = diagnostic * 365.25)) +
  geom_density(fill = "#20A39E", color = "#187370") +
  theme_minimal() +
  labs(y = "Density",
       x = "Time from diagnosis to first faecal calprotectin (days)") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(rows = vars(class))
ggsave("plots/fc-diagnostic-dist-cluster.png",
       p,
       width = 16 * 2/3,
       height = 18 * 2/3,
       units = "in")
ggsave("plots/fc-diagnostic-dist-cluster.pdf",
       p,
       width = 16 * 2/3,
       height = 18 * 2/3,
       units = "in")


crp.dist <- readRDS(paste0(prefix, "processed/crp-diag-dist.RDS"))
model.crp <- readRDS(paste0(prefix, "/cache/crp-ma/crp-8.RDS"))

crp.dist <- subset(crp.dist, ids %in% model.crp$pprob$ids)
crp.dist <- merge(crp.dist, model.crp$pprob[, c("ids", "class")], by = "ids")
crp.dist$class <- paste("CRP", crp.dist$class)
crp.dist$class <- plyr::mapvalues(crp.dist$class,
                                 from = paste0("CRP ", seq(1, 8)),
                                 to = paste0("CRP", c(2, 3, 1, 4, 5, 7, 6, 8)))


p <- crp.dist %>%
  ggplot(aes(x = diagnostic * 365.25)) +
  geom_density(fill = "#415A77", color = "#1B263B") +
  theme_minimal() +
  labs(y = "Density",
       x = "Time from diagnosis to first CRP (days)") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(rows = vars(class))
ggsave("plots/crp-diagnostic-dist-cluster.png",
       p,
       width = 16 * 2/3,
       height = 20 * 2/3,
       units = "in")
ggsave("plots/crp-diagnostic-dist-cluster.pdf",
       p,
       width = 16 * 2/3,
       height = 20 * 2/3,
       units = "in")

