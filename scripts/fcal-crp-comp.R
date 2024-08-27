library(libdr)
library(ComplexHeatmap)
library(ggalluvial)
library(lcmm)
library(patchwork)

set.seed(123)

########################
### Load FCAL models ###
########################
prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
# set the number of groups
G.fcal <- numeric()
models.fcal <- list()
G.cands <- seq(2, 10)
for (G.cand in G.cands) {
  file.name <- paste0(prefix, "/cache/fcal/ncs/fcal-", G.cand, ".RDS")
  if (file.exists(file.name)) {
    G.fcal <- c(G.fcal, G.cand)
    models.fcal[[G.cand]] <- readRDS(file.name)
  }
}
rm(G.cand, G.cands)

dict <- readRDS(paste0(prefix, "processed/dict.RDS"))

####################################################################
### Load CRP moving average (no autocorrelation struture) models ###
####################################################################
prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/cache/crp-ma/"
G.crp <- numeric()
models.crp <- list()
G.cands <- seq(2, 10)
for (G.cand in G.cands) {
  file.name <- paste0(prefix, "crp-", G.cand, ".RDS")
  if (file.exists(file.name)) {
    G.crp <- c(G.crp, G.cand)
    models.crp[[G.cand]] <- readRDS(file.name)
  }
}
rm(G.cand, G.cands)


# Function to create co-clustering matrix for FCAL and CRP models
compareClustering <- function(G,
                              models.fcal,
                              models.crp,
                              cutoff = FALSE,
                              threshold = 0.8) {
  # Extract posterior probabilities
  fcal.pprob <- models.fcal[[G]]$pprob
  crp.pprob <- models.crp[[G]]$pprob

  if (cutoff) {
    ###################
    ### FCAL cutoff ###
    ###################
    inc <- c() # Logical vector used to select rows with pprob above threshold
    for (i in seq_len(nrow(fcal.pprob))) {
      # If posterior probability for assigned class is above threshold
      if (fcal.pprob[i, 2 + fcal.pprob[i, 2]] > threshold) {
        inc <- c(inc, TRUE)
      } else { # Not above threshold
        inc <- c(inc, FALSE)
      }
    }
    fcal.pprob <- fcal.pprob[inc, ] # Reduce to only subjects above threshold


    ##################
    ### CRP cutoff ###
    ##################
    inc <- c()
    for (i in seq_len(nrow(crp.pprob))) {
      if (crp.pprob[i, 2 + crp.pprob[i, 2]] > threshold) {
        inc <- c(inc, TRUE)
      } else {
        inc <- c(inc, FALSE)
      }
    }
    crp.pprob <- crp.pprob[inc, ]
  }

  fcal.ids <- fcal.pprob$ids
  crp.ids <- crp.pprob$ids

  # Only IDs in both FCAL and CRP models
  ids.comb <- fcal.ids[fcal.ids %in% crp.ids]
  ids.comb <- ids.comb[order(ids.comb)]


  # Reduce datasets to only shared IDS
  fcal.prob <- subset(fcal.pprob, ids %in% ids.comb)
  fcal.prob <- fcal.prob[order(fcal.prob$ids), ] # Order by ID

  crp.prob <- subset(crp.pprob, ids %in% ids.comb)
  # Also order by ID (ensures subjects are in same row in both FCAL and CRP)
  crp.prob <- crp.prob[order(crp.prob$ids), ]

  # Matrix to hold co-clustering info
  mat <- matrix(0, nrow = length(ids.comb), ncol = length(ids.comb))
  colnames(mat) <- ids.comb
  rownames(mat) <- ids.comb

  # Matrices for just FCAL and CRP clustering
  mat.fcal <- mat
  mat.crp <- mat

  # FCAL
  for (i in seq_along(ids.comb)) { # Row i
    cluster <- fcal.prob[i, "class"]
    mat.fcal[i, ] <- ifelse(fcal.prob[, "class"] == cluster, 1, 0)
    #for (j in seq_along(ids.comb)) { # Column j
    #  if (fcal.prob[j, "class"] == cluster) mat.fcal[i, j] <- 1
    #} # Else 0
  }

  # CRP
  for (i in seq_along(ids.comb)) { # Row i
    cluster <- crp.prob[i, "class"]
    mat.crp[i, ] <- ifelse(crp.prob[, "class"] == cluster, 1, 0)
   # for (j in seq_along(ids.comb)) { # Column j
   #   if (crp.prob[j, "class"] == cluster) mat.crp[i, j] <- 1
   # } # Else 0
  }


  # Assign values to comparison co-cluster matrix
  # 0 means no co-clustering
  mat[mat.fcal == 1 & mat.crp == 0] <- 2 # Only co-clustered by FCAL
  mat[mat.fcal == 0 & mat.crp == 1] <- 1 # Only co-clustered by CRP
  mat[mat.fcal == 1 & mat.crp == 1] <- 3 # Co-clustered by both FCAL and CRP

  #  mat[mat.fcal == 1 & mat.crp == 0] <- 0.5 # Only co-clustered by FCAL
  #  mat[mat.fcal == 0 & mat.crp == 1] <- 0.5 # Only co-clustered by CRP
  #  mat[mat.fcal == 1 & mat.crp == 1] <- 1 # Co-clustered by both FCAL and CRP
  return(mat)
}


if (!dir.exists("plots/cluster-comp")) dir.create("plots/cluster-comp")

################################
### No posterior prob cutoff ###
################################

col.vec <- c(
  "1" = "#EF3E36",
  "2" = "#17BEBB",
  "3" = "#F4AC45",
  "4" = "#3E5622",
  "5" = "#DDC3D0",
  "6" = "#8FD744FF",
  "7" = "#2A1E5C"
)

col.vec.ibd <- c(
  "Crohn's Disease" = "#052F5F",
  "Ulcerative Colitis" = "#F15BB5",
  "IBDU" = "#329F5B"
)

for (G in 2:7) { # No G = 8 for FCAL
  comp.mat <- compareClustering(G, models.fcal, models.crp)

  ids.fcal <- models.fcal[[G]]$pprob$ids
  ids.crp <- models.crp[[G]]$pprob$ids
  ids.common <- intersect(ids.fcal, ids.crp)

  dict.sub <- subset(dict, ids %in% ids.common)

  column_ha <- HeatmapAnnotation(
    "CRP cluster" = models.crp[[G]]$pprob$class[models.crp[[G]]$pprob$ids %in% ids.common],
    "FCAL cluster" = models.fcal[[G]]$pprob$class[models.fcal[[G]]$pprob$ids %in% ids.common],
    col = list("FCAL cluster" = col.vec, "CRP cluster" = col.vec)
  )

  row.ha <- HeatmapAnnotation(
    "IBD type" = dict.sub$diagnosis,
    col = list("IBD type" = col.vec.ibd),
    annotation_name_rot = 45,
    which = "row"
  )

  heat <- Heatmap(comp.mat,
    name = sprintf("Cluster Concordance"),
    col = gameR::gameR_cols("cyberpunk", reverse = TRUE),
    show_row_names = FALSE,
    show_column_names = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    heatmap_legend_param = list(
      labels = c(
        "Shares cluster for both FCAL and CRP",
        "Shares cluster for only CRP",
        "Shares cluster for only FCAL",
        "Never clustered together"
      )
    ),
    top_annotation = column_ha,
    left_annotation = row.ha,
    use_raster = TRUE,
    raster_quality = 2,
    raster_device = "CairoPNG",
    raster_resize_mat = TRUE,
    raster_by_magick = TRUE
  )
  # Save heatmaps to file
  png(paste0("plots/cluster-comp/G=", G, ".png"),
    width = 10.82,
    height = 6.8 * 1.138947,
    units = "in",
    res = 300
  )
  draw(heat)
  dev.off()

  pdf(paste0("plots/cluster-comp/G=", G, ".pdf"),
    width = 10.82,
    height = 6.8 * 1.138947
  )
  draw(heat)
  dev.off()
}


##################################
### With posterior prob cutoff ###
##################################

col.vec <- c(
  "1" = "#EF3E36",
  "2" = "#17BEBB",
  "3" = "#F4AC45",
  "4" = "#3E5622",
  "5" = "#DDC3D0",
  "6" = "#8FD744FF",
  "7" = "#2A1E5C"
)

col.vec.ibd <- c(
  "Crohn's Disease" = "#052F5F",
  "Ulcerative Colitis" = "#F15BB5",
  "IBDU" = "#329F5B"
)

for (G in 2:7) {
  comp.mat <- compareClustering(G, models.fcal, models.crp, cutoff = TRUE)

  ids.fcal <- models.fcal[[G]]$pprob[apply(models.fcal[[G]]$pprob[,c(-1, -2)] > 0.8, 1, any),]$ids
  ids.crp <- models.crp[[G]]$pprob[apply(models.crp[[G]]$pprob[,c(-1, -2)] > 0.8, 1, any),]$ids

  ids.common <- intersect(ids.fcal, ids.crp)

  dict.sub <- subset(dict, ids %in% ids.common)


  column_ha <- HeatmapAnnotation(
    "CRP cluster" = models.crp[[G]]$pprob$class[models.crp[[G]]$pprob$ids %in% ids.common],
    "FCAL cluster" = models.fcal[[G]]$pprob$class[models.fcal[[G]]$pprob$ids %in% ids.common],
    col = list("FCAL cluster" = col.vec, "CRP cluster" = col.vec)
  )

  row.ha <- HeatmapAnnotation(
    "IBD type" = dict.sub$diagnosis,
    col = list("IBD type" = col.vec.ibd),
    annotation_name_rot = 45,
    which = "row"
  )


  heat <- Heatmap(comp.mat,
    name = paste0("Cluster Concordance, n = ", nrow(comp.mat)),
    col = gameR::gameR_cols("cyberpunk", reverse = TRUE),
    show_row_names = FALSE,
    show_column_names = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    heatmap_legend_param = list(
      labels = c(
        "Shares cluster for both FCAL and CRP",
        "Shares cluster for only CRP",
        "Shares cluster for only FCAL",
        "Never clustered together"
      )
    ),
    top_annotation = column_ha,
    left_annotation = row.ha,
    use_raster = TRUE,
    raster_quality = 2,
    raster_device = "CairoPNG",
    raster_resize_mat = TRUE,
    raster_by_magick = TRUE
  )
  # Save heatmaps to file
  png(paste0("plots/cluster-comp/G=", G, "-pprob.png"),
    width = 10.82,
    height = 8,
    units = "in",
    res = 300
  )
  draw(heat)
  dev.off()

  pdf(paste0("plots/cluster-comp/G=", G, "-pprob.pdf"),
    width = 10.82,
    height = 8
  )
  draw(heat)
  dev.off()
}

fcal.pprob <- models.fcal[[6]]$pprob
crp.pprob <- models.crp[[8]]$pprob

fcal.ids <- fcal.pprob$ids
crp.ids <- crp.pprob$ids

# Only IDs in both FCAL and CRP models
ids.comb <- fcal.ids[fcal.ids %in% crp.ids]
ids.comb <- ids.comb[order(ids.comb)]

fcal.pprob <- subset(fcal.pprob, ids %in% ids.comb)
crp.pprob <- subset(crp.pprob, ids %in% ids.comb)


classes <- rbind(data.frame(fcal.pprob[, c(1,2)], type = "FC" ),
                 data.frame(crp.pprob[, c(1,2)], type = "CRP"))
classes$class <- as.factor(classes$class)
classes$type <- as.factor(classes$type)
classes$type <- relevel(classes$type, "FC")

p1 <- ggplot(classes, aes(x = type,
           stratum = class,
           alluvium = ids,
           fill = class,
           label = class)) +
  geom_flow()  +
  geom_stratum()  + geom_text(stat = "stratum", size = 3) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "gray")) +
  xlab("Biomarker")


calpro_time <- seq(0, 7, by = 0.1)

new.data <- cbind(calpro_time, ns(calpro_time, df = 4, Boundary.knots = c(0,7)))
new.data <- rbind(data.frame(new.data, diagnosis = "Crohn's"), data.frame(new.data, diagnosis = "UC"))


preds.fcal <- predictY(models.fcal[[6]], newdata = new.data, draws = TRUE)
plot.fcal <- data.frame(time = calpro_time, pred = preds.fcal$pred[, "Ypred_class1"], class = "1" )
for (class in as.character(2:6)){
  plot.fcal <- rbind(plot.fcal,
                     data.frame(time = calpro_time, pred = preds.fcal$pred[, paste0("Ypred_class",class)], class = class))
}

p2 <- ggplot(plot.fcal, aes(x = time, y = pred, color = class)) +
  geom_line(size = 0.7) +
  theme_minimal() +
  xlab("Time (years)") +
  ylab("Log(FC (µg/g))") +
  ylim(0, 7) +
  xlim(0, 7) +
  theme(axis.line = element_line(colour = "gray"))



crp_time <- seq(0,7, by = 0.1)

new.data <- cbind(crp_time, ns(crp_time, df = 4, Boundary.knots = c(0,7)))
new.data <- rbind(data.frame(new.data, diagnosis = "Crohn's"),
                  data.frame(new.data, diagnosis = "UC"))

preds.crp <- predictY(models.crp[[8]], newdata = new.data, draws = TRUE)
plot.crp <- data.frame(time = crp_time, pred = preds.crp$pred[, "Ypred_class1"], class = "1" )
for (class in as.character(2:8)){
  plot.crp <- rbind(
    plot.crp,
    data.frame(time = crp_time,
               pred = preds.crp$pred[, paste0("Ypred_class",class)],
               class = class)
    )
}

p3 <- ggplot(plot.crp, aes(x = time, y = pred, color = class)) +
  geom_line(size = 0.7) +
  theme_minimal() +
  xlab("Time (years)") +
  ylab("Log(CRP (mg/L))") +
  ylim(0, 7) +
  xlim(0, 7) +
  theme(axis.line = element_line(colour = "gray"))


p <- p1 + (p2/p3) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position='bottom') &
  guides(fill = guide_legend(nrow = 1))

p

ggsave("paper/big-comp.png", p, width = 10, height = 6.75)
ggsave("paper/big-comp.pdf", p, width = 10, height = 6.75)

