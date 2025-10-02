## ----Setup---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
## Modelling ##
library(lcmm)
library(kml) # K-means
## Presentation ##
library(patchwork)
library(ggdist)
library(ggalluvial)
library(pander)
library(qqplotr)
library(Cairo) # For graphics device

##########################
#--     Data read      --#
##########################

dict <- readRDS(paste0(prefix, "processed/dict.RDS"))
fcal <- readRDS(paste0(prefix, "processed/fcal.RDS"))

dk.fcal <- read.csv(paste0(prefix, "Denmark/2024-11-29/Fcal_8_models.csv"),
                    sep = ";")


## ----Load NCS FCAL models------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# set the number of groups
G.fcal <- numeric()
models.fcal <- list()
G.cands <- seq(2, 11)
for (G.cand in G.cands) {
  file.name <- paste0(prefix, "/cache/fcal/nodiag/fcal-", G.cand, ".RDS")
  if (file.exists(file.name)) {
    G.fcal <- c(G.fcal, G.cand)
    models.fcal[[G.cand]] <- readRDS(file.name)
  }
}
rm(G.cand)

## ----FCAL postpro--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| results: "asis"
postprob_quiet <- quietly(postprob)
for (G in G.fcal) {
  cat(paste0("##### G = ", G, "\n"))
  output <- postprob_quiet(models.fcal[[G]])$result
  cat(pander::pander(output[[1]]))
  cat(pander::pander(output[[2]]))
  cat(pander::pander(output[[3]]))
}
rm(output)


## ----FCAL residuals------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| output: "asis"
for (G in G.fcal) {
  cat(paste0("##### G = ", G, "\n"))
  plot(models.fcal[[G]], shades = TRUE)
}


## ----Assess FCAL NCS normality-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-NCS-norm
#| fig-cap: "Plots assessing normality for the distribution of residuals for the chosen NCS model for FCAL. (A) Histrogram; (B) Q-Q plot."
p1 <- data.frame(residuals = resid(models.fcal[[8]])) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)),
    fill = "#D8829D",
    color = "#AF6A80",
    bins = 30
  ) +
  geom_density(color = "#023777", linewidth = 1.2) +
  theme_classic() +
  theme(axis.line = element_line(colour = "gray")) +
  ylab("Density") +
  xlab("Residuals") +
  ggtitle("A")

p2 <- data.frame(residuals = resid(models.fcal[[8]])) %>%
  ggplot(aes(sample = residuals)) +
  stat_qq_band() +
  stat_qq_line(color = "#D8829D") +
  stat_qq_point(color = "#023777") +
  theme_classic() +
  theme(axis.line = element_line(colour = "gray")) +
  ylab("Theoretical Quantiles") +
  xlab("Sample Quantiles") +
  ggtitle("B")

# Use Cairo for safe PDF output
tryCatch({
  Cairo::CairoPDF("paper/Residual-plot-fcal-nodiag.pdf",
                  width = 8,
                  height = 8)
  print(p1 / p2)
  dev.off()
  cat("Residual plot PDF saved successfully with Cairo\n")
}, error = function(e) {
  if (!is.null(dev.list())) dev.off()
  cat("Residual plot PDF Cairo approach failed:", e$message, "\n")
})

print(p1 / p2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-NCS-norm-cluster
#| fig-cap: "Q-Q plots assessing normality for the distribution of residuals stratifed by cluster assignment."
predClass <- models.fcal[[8]]$pprob[, c("ids", "class")]
temp <- merge(models.fcal[[8]]$pred, predClass, by = "ids")
labels <- c("A", "B", "C", "D", "E", "F", "G", "H")
p <- list()

for (i in 1:8) {
  p[[i]] <- subset(temp, class == i) %>%
    ggplot(aes(sample = resid_ss)) +
    stat_qq_band() +
    stat_qq_line(color = "#D8829D") +
    stat_qq_point(color = "#023777") +
    theme_classic() +
    theme(axis.line = element_line(colour = "gray")) +
    ylab("Theoretical Quantiles") +
    xlab("Sample Quantiles") +
    ggtitle(labels[i])
}

ArrangedPlot <- (p[[1]] + p[[2]]) /
  (p[[3]] + p[[4]]) /
  (p[[5]] + p[[6]]) /
  (p[[7]] + p[[8]])

# Use Cairo for safe PDF output
tryCatch({
  Cairo::CairoPDF("paper/cluster-resids-ncs.pdf",
                  width = 8,
                  height = 8)
  print(ArrangedPlot)
  dev.off()
  cat("Cluster residuals PDF saved successfully with Cairo\n")
}, error = function(e) {
  if (!is.null(dev.list())) dev.off()
  cat("Cluster residuals PDF Cairo approach failed:", e$message, "\n")
})

print(ArrangedPlot)



## ----FCAL spaghetti plots------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (G in G.fcal) {
  # Data frame to hold processed data
  new.fc <- data.frame(
    ids = numeric(),
    calpro_result = numeric(),
    calpro_time = numeric(),
    class = numeric()
  )

  for (clust in 1:G) {
    ids.clust <- subset(models.fcal[[G]]$pprob, class == clust)$ids
    n.clust <- length(ids.clust)
    rand <- sample(n.clust, n.clust) # Randomise the order of the ids
    iters <- floor(n.clust / 6) # How many groups of six are there?

    # Matrix to hold the smoothed data
    fcal.ma <- matrix(NA, nrow = iters, ncol = 7)
    fcal.time <- matrix(NA, nrow = iters, ncol = 7)


    for (i in 0:(iters - 1)) {
      # Find ids for group of five
      ids.select <- ids.clust[rand[((i * 6) + 1):((i * 6) + 6)]]
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
          fcal.ma[i + 1, j + 1] <- median(sub.obs$calpro_result)
          fcal.time[i + 1, j + 1] <- median(sub.obs$calpro_time)
        }
      }
    }

    rownames(fcal.ma) <- 1:iters
    fcal.ma <- reshape2::melt(t(fcal.ma),
      id.vars = row.names(fcal.ma),
      na.rm = TRUE
    )
    colnames(fcal.ma) <- c("calpro_time", "ids", "calpro_result")
    fcal.ma <- fcal.ma[, c(2, 3, 1)] # Make ids first column


    rownames(fcal.time) <- 1:iters
    fcal.time <- reshape2::melt(t(fcal.time),
      id.vars = row.names(fcal.time),
      na.rm = TRUE
    )
    colnames(fcal.time) <- c("calpro_time", "ids", "pred_time")
    fcal.time <- fcal.time[, c(2, 3, 1)] # Make ids first column

    fcal.ma <- merge(fcal.ma, fcal.time, by = c("ids", "calpro_time"))
    fcal.ma <- fcal.ma %>%
      mutate(calpro_time = pred_time) %>%
      select(-pred_time)

    fcal.ma$class <- clust # Identify cluster assignment
    new.fc <- rbind(new.fc, fcal.ma)
  }

  if (!dir.exists("plots/spaghetti")) dir.create("plots/spaghetti")
  png(paste0("plots/spaghetti/fcal-nodiag-", G, ".png"),
    width = 10,
    height = 17.5,
    units = "in",
    res = 300
  )
  grid::grid.newpage()
  spaghettiPlot(new.fc,
    models.fcal,
    G = G,
    log = TRUE,
    tmax = 7,
    sizes = TRUE,
    knots = FALSE,
    var.time = "calpro_time",
    clusters = TRUE
  )
  invisible(dev.off())

  cairo_pdf(paste0("plots/spaghetti/fcal-nodiag-", G, ".pdf"),
    width = 10,
    height = 17.5
  )
  grid::grid.newpage()
  spaghettiPlot(new.fc,
    models.fcal,
    G = G,
    log = TRUE,
    tmax = 7,
    sizes = TRUE,
    knots = FALSE,
    var.time = "calpro_time",
    clusters = TRUE
  )
  invisible(dev.off())

  grid::grid.newpage()
  print(spaghettiPlot(new.fc,
    models.fcal,
    G = G,
    log = TRUE,
    tmax = 7,
    sizes = TRUE,
    knots = FALSE,
    var.time = "calpro_time",
    clusters = TRUE
  ))
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
new.fc <- data.frame(
  ids = numeric(),
  calpro_result = numeric(),
  calpro_time = numeric(),
  class = numeric()
)

for (clust in 1:8) {
  ids.clust <- subset(models.fcal[[8]]$pprob, class == clust)$ids
  n.clust <- length(ids.clust)
  rand <- sample(n.clust, n.clust) # Randomise the order of the ids
  iters <- floor(n.clust / 6) # How many groups of six are there?

  # Matrix to hold the smoothed data
  fcal.ma <- matrix(NA, nrow = iters, ncol = 7)
  fcal.time <- matrix(NA, nrow = iters, ncol = 7)


  for (i in 0:(iters - 1)) {
    # Find ids for group of five
    ids.select <- ids.clust[rand[((i * 6) + 1):((i * 6) + 6)]]
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
        fcal.ma[i + 1, j + 1] <- median(sub.obs$calpro_result)
        fcal.time[i + 1, j + 1] <- median(sub.obs$calpro_time)
      }
    }
  }

  rownames(fcal.ma) <- 1:iters
  fcal.ma <- reshape2::melt(t(fcal.ma),
    id.vars = row.names(fcal.ma),
    na.rm = TRUE
  )
  colnames(fcal.ma) <- c("calpro_time", "ids", "calpro_result")
  fcal.ma <- fcal.ma[, c(2, 3, 1)] # Make ids first column


  rownames(fcal.time) <- 1:iters
  fcal.time <- reshape2::melt(t(fcal.time),
    id.vars = row.names(fcal.time),
    na.rm = TRUE
  )
  colnames(fcal.time) <- c("calpro_time", "ids", "pred_time")
  fcal.time <- fcal.time[, c(2, 3, 1)] # Make ids first column

  fcal.ma <- merge(fcal.ma, fcal.time, by = c("ids", "calpro_time"))
  fcal.ma <- fcal.ma %>%
    mutate(calpro_time = pred_time) %>%
    select(-pred_time)

  fcal.ma$class <- clust # Identify cluster assignment
  new.fc <- rbind(new.fc, fcal.ma)
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rank.full <- rankCumulative(models.fcal[[8]], tmax = 7)

rank.full %>%
  ggplot(aes(x = paste0("FC", New), y = Area)) +
  geom_bar(stat = "identity", fill = "#745C97", color = "#39375B") +
  ylab("Cumulative inflammation (Area)") +
  xlab("Cluster") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
png(paste0("plots/spaghetti/fcal-nodiag-reordered.png"),
  width = 10,
  height = 16,
  units = "in",
  res = 300
)
grid::grid.newpage()
spaghettiPlot(new.fc,
  models.fcal,
  G = 8,
  log = TRUE,
  tmax = 7,
  sizes = TRUE,
  knots = FALSE,
  var.time = "calpro_time",
  clusters = TRUE,
  mapping = rank.full$Original
)
invisible(dev.off())

cairo_pdf(paste0("plots/spaghetti/fcal-nodiag-reordered.pdf"),
  width = 10,
  height = 16
)
grid::grid.newpage()
spaghettiPlot(new.fc,
  models.fcal,
  G = 8,
  log = TRUE,
  tmax = 7,
  sizes = TRUE,
  knots = FALSE,
  var.time = "calpro_time",
  clusters = TRUE,
  mapping = rank.full$Original
)
invisible(dev.off())

spaghettiPlot(new.fc,
  models.fcal,
  G = 8,
  log = TRUE,
  tmax = 7,
  sizes = TRUE,
  knots = FALSE,
  var.time = "calpro_time",
  clusters = TRUE,
  mapping = rank.full$Original
)

original <- readRDS(paste0(prefix, "cache/fcal/ncs/fcal-8.RDS"))$pprob
no.diag <- models.fcal[[8]]$pprob


table(no.diag$class)
no.diag.clust.new <- subset(no.diag, !(ids %in% original$pprob$ids))



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

cairo_pdf("plots/fcal-nodiag-pprob.pdf",
            width = 10,
            height = 7)
p
invisible(dev.off())
p

## ----Session info--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pander(sessionInfo())
