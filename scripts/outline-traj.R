library(lcmm)
suppressPackageStartupMessages(library(tidyverse))
library(patchwork)
library(splines)

set.seed(123)
if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/cvallejo-predicct/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}

model.fc <- readRDS(paste0(prefix, "/cache/fcal/ncs/fcal-8.RDS"))

pred.fc.df <- data.frame(
  calpro_time = c(seq(0, 7, 0.01), seq(0, 7, 0.01)),
  diagnosis = c(
    rep("Crohn's Disease", 701),
    rep("Ulcerative Colitis", 701)
  )
)
pred.fc.df.update <- predictY(model.fc,
                              pred.fc.df,
                              var.time = "calpro_time",
                              draws = TRUE
)$pred

temp <- data.frame(Time = NULL, Cluster = NULL, Value = NULL)

for (g in 1:8) {
  temp <- rbind(
    temp,
    data.frame(
      Time = seq(0, 7, 0.01),
      Cluster = g,
      Value = pred.fc.df.update[, g]
    )
  )
}

traj.outlines <- list()

for (g in 1:8) {
  traj.outlines[[g]] <- temp %>%
    filter(Cluster == g) %>%
    ggplot(aes(x = Time, y = Value)) +
    geom_line(color = "#757575") +
    theme_classic() +
    theme(text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(color = "#dfdfdf")) +
    ylim(3, 6.5)
}

p <- traj.outlines[[1]] +
  traj.outlines[[2]] +
  traj.outlines[[3]] +
  traj.outlines[[4]] +
  traj.outlines[[5]] +
  traj.outlines[[6]] +
  traj.outlines[[7]] +
  traj.outlines[[8]] + plot_layout(ncol = 8)

ggsave("plots/fc-traj-outline.pdf",
       p,
       width = 8 * 1.65,
       height = 1 * 1.65
)
ggsave("plots/fc-traj-outline.png",
       p,
       width = 8 * 1.65,
       height = 1 * 1.65
)


model.crp <- readRDS(paste0(prefix, "/cache/crp-ma/crp-8.RDS"))



pred.crp.df <- data.frame(
  crp_time = c(seq(0, 6.25, 0.01), seq(0, 6.25, 0.01)),
  diagnosis = c(
    rep("Crohn's Disease", 626),
    rep("Ulcerative Colitis", 626)
  )
)
pred.crp.df.update <- predictY(model.crp,
                              pred.crp.df,
                              var.time = "crp_time",
                              draws = TRUE
)$pred

temp <- data.frame(Time = NULL, Cluster = NULL, Value = NULL)

for (g in 1:8) {
  temp <- rbind(
    temp,
    data.frame(
      Time = seq(0, 6.25, 0.01),
      Cluster = g,
      Value = pred.crp.df.update[, g]
    )
  )
}

traj.outlines <- list()

for (g in 1:8) {
  traj.outlines[[g]] <- temp %>%
    filter(Cluster == g) %>%
    ggplot(aes(x = Time, y = Value)) +
    geom_line(color = "#757575") +
    theme_classic() +
    theme(text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(color = "#dfdfdf")) +
    ylim(0, 4.2)
}

p <- traj.outlines[[1]] +
  traj.outlines[[2]] +
  traj.outlines[[3]] +
  traj.outlines[[4]] +
  traj.outlines[[5]] +
  traj.outlines[[6]] +
  traj.outlines[[7]] +
  traj.outlines[[8]] +
  plot_layout(ncol = 8)

ggsave("plots/crp-traj-outline.pdf",
       p,
       width = 8 * 1.65,
       height = 1 * 1.65
)
ggsave("plots/crp-traj-outline.png",
       p,
       width = 8 * 1.65,
       height = 1 * 1.65
)

