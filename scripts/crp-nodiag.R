## ----Setup----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| message: false
set.seed(123)
if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/cvallejo-predicct/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}


##########################
#--     Packages       --#
##########################

library(tidyverse)
# Support package (source found in libdr/)
library(libdr)
## Graphics ##
library(Cairo) # For stable graphics rendering
## Modelling ##
library(lcmm)
library(kml) # K-means
## Presentation ##
library(patchwork)
library(ggdist)
library(ggalluvial)
library(pander)
library(qqplotr)


##########################
#--     Data read      --#
#########################

dict <- readRDS(paste0(prefix, "processed/dict.RDS"))
crp_median <- readRDS(paste0(prefix, "processed/median-crp-nodiag.RDS"))
crp <- readRDS(paste0(prefix, "processed/crp-nodiag.RDS"))


# Find best model across 10 random seeds
for (seed in 1:10) {
  file.name <- paste0(prefix, "/cache/crp-ma/nodiag/seeds/crp-8-", seed, ".RDS")
  if (file.exists(file.name)) {
    model.seed <- readRDS(file.name)
    if (seed == 1) {
      best.model.seed <- model.seed
    } else {
      if (model.seed$loglik > best.model.seed$loglik) {
        best.model.seed <- model.seed
      }
    }
  }
}


# Mirror structure from original script
G.crp <- 8
models.crp.ma <- list()
models.crp.ma[[8]] <- best.model.seed



## ----CRP-ma postpro-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "asis"
postprob_quiet <- quietly(postprob)
for (G in G.crp) {
  cat(paste0("##### G = ", G, "\n"))
  output <- postprob_quiet(models.crp.ma[[G]])$result
  cat(pander::pander(output[[1]]))
  cat(pander::pander(output[[2]]))
  cat(pander::pander(output[[3]]))
}
rm(output)

## ----CRP MA residuals-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "asis"
for (G in G.crp) {
  cat(paste0("##### G = ", G, "\n"))
  plot(models.crp.ma[[G]], shades = TRUE)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (G in G.crp) {
  # Data frame to hold processed data
  new.crp <- data.frame(
    ids = numeric(),
    crp_result = numeric(),
    crp_time = numeric(),
    class = numeric()
  )

  for (clust in 1:G) {
    ids.clust <- subset(models.crp.ma[[G]]$pprob, class == clust)$ids
    n.clust <- length(ids.clust)
    rand <- sample(n.clust, n.clust) # Randomise the order of the ids
    iters <- floor(n.clust / 6) # How many groups of six are there?

    # Matrix to hold the smoothed data
    crp.ma <- matrix(NA, nrow = iters, ncol = 7)
    for (i in 0:(iters - 1)) {
      # Find ids for group of five
      ids.select <- ids.clust[rand[((i * 6) + 1):((i * 6) + 6)]]
      crp.subset <- subset(crp_median, ids %in% ids.select)
      # Median process as per CRP preprocessing
      for (j in seq(0, 6)) {
        if (j == 6) {
          sub.obs <- subset(
            crp.subset,
            crp_time >= j - 0.5 & crp_time <= j + 1
          )
        } else {
          sub.obs <- subset(
            crp.subset,
            crp_time >= j - 0.5 & crp_time < j + 0.5
          )
        }
        if (nrow(sub.obs) > 0) {
          crp.ma[i + 1, j + 1] <- median(sub.obs$crp_result)
        }
      }
    }

    rownames(crp.ma) <- 1:iters
    crp.ma <- reshape2::melt(t(crp.ma),
      id.vars = row.names(crp.ma),
      na.rm = TRUE
    )
    colnames(crp.ma) <- c("crp_time", "ids", "crp_result")
    crp.ma <- crp.ma[, c(2, 3, 1)] # Make ids first column
    crp.ma$crp_time <- crp.ma$crp_time - 1
    # Take into account uneven spacing at start and end
    crp.ma$crp_time <- plyr::mapvalues(crp.ma$crp_time,
      from = c(0, 6),
      to = c(0.25, 6.25)
    )
    crp.ma$class <- clust # Identify cluster assignment
    new.crp <- rbind(new.crp, crp.ma)
  }

  cairo_pdf(paste0("plots/spaghetti/crp-nodiag-", G, ".pdf"),
    width = 10,
    height = 17.5
  )
  grid::grid.newpage()
  spaghettiPlot(new.crp,
    models.crp.ma,
    G,
    clusters = TRUE,
    tmax = 6.25,
    sizes = TRUE,
    var.time = "crp_time",
    ylim = "data"
  )
  invisible(dev.off())

  png(paste0("plots/spaghetti/crp-nodiag-", G, ".png"),
    width = 10,
    height = 17.5,
    units = "in",
    res = 300
  )
  grid::grid.newpage()
  spaghettiPlot(new.crp,
    models.crp.ma,
    G,
    clusters = TRUE,
    tmax = 6.25,
    sizes = TRUE,
    var.time = "crp_time",
    ylim = "data"
  )
  invisible(dev.off())

  grid::grid.newpage()
  spaghettiPlot(new.crp,
    models.crp.ma,
    G,
    clusters = TRUE,
    tmax = 6.25,
    sizes = TRUE,
    var.time = "crp_time",
    ylim = "data"
  )
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
new.crp <- data.frame(
  ids = numeric(),
  crp_result = numeric(),
  crp_time = numeric(),
  class = numeric()
)

for (clust in 1:8) {
  ids.clust <- subset(models.crp.ma[[8]]$pprob, class == clust)$ids
  n.clust <- length(ids.clust)
  rand <- sample(n.clust, n.clust) # Randomise the order of the ids
  iters <- floor(n.clust / 6) # How many groups of six are there?

  # Matrix to hold the smoothed data
  crp.ma <- matrix(NA, nrow = iters, ncol = 7)
  for (i in 0:(iters - 1)) {
    # Find ids for group of five
    ids.select <- ids.clust[rand[((i * 6) + 1):((i * 6) + 6)]]
    crp.subset <- subset(crp_median, ids %in% ids.select)
    # Median process as per CRP preprocessing
    for (j in seq(0, 6)) {
      if (j == 6) {
        sub.obs <- subset(
          crp.subset,
          crp_time >= j - 0.5 & crp_time <= j + 1
        )
      } else {
        sub.obs <- subset(
          crp.subset,
          crp_time >= j - 0.5 & crp_time < j + 0.5
        )
      }
      if (nrow(sub.obs) > 0) {
        crp.ma[i + 1, j + 1] <- median(sub.obs$crp_result)
      }
    }
  }

  rownames(crp.ma) <- 1:iters
  crp.ma <- reshape2::melt(t(crp.ma),
    id.vars = row.names(crp.ma),
    na.rm = TRUE
  )
  colnames(crp.ma) <- c("crp_time", "ids", "crp_result")
  crp.ma <- crp.ma[, c(2, 3, 1)] # Make ids first column
  crp.ma$crp_time <- crp.ma$crp_time - 1
  # Take into account uneven spacing at start and end
  crp.ma$crp_time <- plyr::mapvalues(crp.ma$crp_time,
    from = c(0, 6),
    to = c(0.25, 6.25)
  )
  crp.ma$class <- clust # Identify cluster assignment
  new.crp <- rbind(new.crp, crp.ma)
}

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rank.full <- rankCumulative(models.crp.ma[[8]],
  tmax = 6.25,
  var.time = "crp_time"
)
rank.full %>%
  ggplot(aes(x = paste0("CRP", New), y = Area)) +
  geom_bar(stat = "identity", fill = "#745C97", color = "#39375B") +
  ylab("Cumulative inflammation (Area)") +
  xlab("Cluster") +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
png(paste0("plots/spaghetti/crp-nodiag-reordered.png"),
  width = 10,
  height = 16,
  units = "in",
  res = 300
)
grid::grid.newpage()
spaghettiPlot(new.crp,
  models.crp.ma,
  G = 8,
  log = TRUE,
  tmax = 6.25,
  sizes = TRUE,
  knots = FALSE,
  var.time = "crp_time",
  clusters = TRUE,
  ylim = "data",
  mapping = rank.full$Original
)
invisible(dev.off())

cairo_pdf(paste0("plots/spaghetti/crp-nodiag-reordered.pdf"),
  width = 10,
  height = 16
)
grid::grid.newpage()
spaghettiPlot(new.crp,
  models.crp.ma,
  G = 8,
  log = TRUE,
  tmax = 6.25,
  sizes = TRUE,
  knots = FALSE,
  var.time = "crp_time",
  clusters = TRUE,
  ylim = "data",
  mapping = rank.full$Original
)
invisible(dev.off())

spaghettiPlot(new.crp,
  models.crp.ma,
  G = 8,
  log = TRUE,
  tmax = 6.25,
  sizes = TRUE,
  knots = FALSE,
  var.time = "crp_time",
  clusters = TRUE,
  ylim = "data",
  mapping = rank.full$Original
)


original <- readRDS(paste0(prefix, "cache/crp-ma/crp-8.RDS"))$pprob
no.diag <- models.crp.ma[[8]]$pprob


table(no.diag$class)
no.diag.clust.new <- subset(no.diag, !(ids %in% original$ids))


# % of each cluster not in diagnostic model.
round((table(no.diag.clust.new$class) / table(no.diag$class)) * 100, 2)

hist.df <- data.frame(pprob = apply(no.diag[, 3:10], 1, max),
                      type = "Previously excluded")

hist.df <- rbind(hist.df,
                 data.frame(pprob = apply(original[, 3:10], 1, max),
                            type = "Original"))

p <- hist.df %>%
  ggplot(aes(x = pprob, fill = type)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#D8829D", "#20A39E")) +
  theme_minimal() +
  labs(y = "Density",
       x = "Maximum posterior probability") +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.8))

cairo_pdf("plots/crp-nodiag-pprob.pdf",
          width = 10,
          height = 7)
p
invisible(dev.off())
p



original <- original %>%
  mutate(class_order = plyr::mapvalues(
    class,
    from = seq_len(8), to = c(2, 3, 1, 4, 5, 7, 6, 8)
  )) %>%
  mutate(class_order = factor(
    class_order,
    levels = 1:8, labels = paste0("CRP", 1:8)
  ))


temp <- original$class_order %>% table()

original.percent <- data.frame(x = names(temp),
                               y = as.numeric(temp) / sum(as.numeric(temp)),
                               type = "Original")


p1 <- original.percent %>%
  ggplot(aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#D8829D", color = "#AF6A80") +
  theme_minimal() +
  labs(x = "Cluster",
       y = "Proportion of cohort") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.45))



no.diag <- no.diag %>%
  mutate(class_order = plyr::mapvalues(
    class,
    from = seq_len(8), to = c(4, 6, 7, 1, 2, 5, 8, 3)
  )) %>%
  mutate(class_order = factor(
    class_order,
    levels = 1:8,
    labels = paste0("CRP", 1:8)
  )
  )

temp2 <- no.diag$class_order %>% table()

no.diag.percent <- data.frame(x = names(temp2),
                              y = as.numeric(temp2) / sum(as.numeric(temp2)),
                              type = "Previously excluded")

p2 <- no.diag.percent %>%
  ggplot(aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#20A39E", color =  "#05817D") +
  theme_minimal() +
  labs(x = "Cluster",
       y = "Proportion of cohort") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.45))

p <- p2/p1 + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))


cairo_pdf("plots/crp-nodiag-cluster-prop.pdf",
          width = 10,
          height = 10)
p
invisible(dev.off())



## ----Session info---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pander(sessionInfo())

