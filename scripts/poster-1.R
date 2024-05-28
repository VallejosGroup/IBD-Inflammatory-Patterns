cairo_pdf("temp.pdf", width = 8, height = 11)
p <-spaghettiPlot(fcal,
              models.fcal,
              G = 6,
              log = TRUE,
              tmax = 7,
              sizes = TRUE,
              save = FALSE,
              knots = FALSE,
              var.time = "calpro_time")
dev.off()

ids <- unique(fcal$ids)
tab2 <- matrix(ncol = 2, nrow = 0)
colnames(tab2) <- c("ids", "Followup")
for (id in ids){
  followup <- max(subset(fcal, ids == id)$calpro_time)
  tab2 <- rbind(tab2, c(id, followup))
}

tab2 <- as.data.frame(tab2)


ids <- unique(crp$CHI)
tab2 <- matrix(ncol = 2, nrow = 0)
colnames(tab2) <- c("ids", "Followup")
for (id in ids){
  followup <- max(subset(crp, CHI == id)$time)
  tab2 <- rbind(tab2, c(id, followup))
}

tab2 <- as.data.frame(tab2)
