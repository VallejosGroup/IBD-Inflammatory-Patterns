## Five-individual median trends ##
library(libdr)
library(lcmm)

set.seed(123)
if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/cvallejo-predicct/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}

dict <- readRDS(paste0(prefix, "processed/dict.RDS"))
fcal <- readRDS(paste0(prefix, "processed/fcal.RDS"))

models.fcal <- list()

models.fcal[[8]] <- readRDS(paste0(prefix, "/cache/fcal/ncs/fcal-8.RDS"))

cluster <- numeric()

dict.fcal <- subset(dict, ids %in% unique(fcal$ids))

for (id in dict.fcal$ids) {
  cluster <- c(
    cluster,
    subset(models.fcal[[8]]$pprob, ids == id)$class
  )
}


dict.fcal$cluster <- as.factor(cluster)

# Data frame to hold processed data
new.fc <- data.frame(ids = numeric(),
                     calpro_result = numeric(),
                     calpro_time = numeric(),
                     class = numeric())

for (clust in 1:8) {
  ids.clust <- subset(dict.fcal, cluster == clust)$ids
  n.clust <- length(ids.clust)
  rand <- sample(n.clust, n.clust) # Randomise the order of the ids
  iters <- floor(n.clust / 5) # How many groups of five are there?

  # Matrix to hold the smoothed data
  fcal.ma <- matrix(NA, nrow = iters, ncol = 7)
  for (i in 0:(iters - 1)) {
    # Find ids for group of five
    ids.select <- ids.clust[rand[((i * 5) + 1):((i * 5) + 5)]]
    fcal.subset <- subset(fcal, ids %in% ids.select)
    # Median process as per CRP preprocessing
    for (j in seq(0, 6)) {
      if (j == 6) {
        sub.obs <- subset(
          fcal.subset,
          calpro_time >= j - 0.5 & calpro_time <= j + 1
        )
      } else {
        sub.obs <- subset(
          fcal.subset,
          calpro_time >= j - 0.5 & calpro_time < j + 0.5
        )
      }
      if (nrow(sub.obs) > 0) {
        fcal.ma[i+1, j + 1] <- median(sub.obs$calpro_result)
      }
    }
  }

  rownames(fcal.ma) <- 1:iters
  fcal.ma <- reshape2::melt(t(fcal.ma ),
                            id.vars = row.names(fcal.ma),
                            na.rm = TRUE)
  colnames(fcal.ma) <- c("calpro_time", "ids", "calpro_result")
  fcal.ma  <- fcal.ma[, c(2, 3, 1)] # Make ids first column
  fcal.ma$calpro_time <- fcal.ma$calpro_time - 1
  # Take into account uneven spacing at start and end
  fcal.ma$calpro_time <- plyr::mapvalues(fcal.ma$calpro_time,
                                         from = c(0, 6),
                                         to = c(0.25, 6.25))
  fcal.ma$class <- clust # Identify cluster assignment
  new.fc <- rbind(new.fc, fcal.ma)
}

cairo_pdf("paper/NCS-8-cluster-smoothed.pdf", width = 7, height = 9)
grid::grid.newpage()
spaghettiPlot(new.fc,
              models.fcal,
              8,
              clusters = TRUE,
              tmax = 7,
              sizes = TRUE)
invisible(dev.off())

png("paper/NCS-8-cluster-smoothed.png",
    width = 7,
    height = 9,
    units = "in",
    res = 300)
grid::grid.newpage()
spaghettiPlot(new.fc,
              models.fcal,
              8,
              clusters = TRUE,
              tmax = 7,
              sizes = TRUE)
invisible(dev.off())
