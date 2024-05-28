set.seed(123)
library(lcmm)
library(libdr)
prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
dict <- readRDS(paste0(prefix, "processed/dict.RDS"))
fcal <- readRDS(paste0(prefix, "processed/fcal.RDS"))
crp <- readRDS(paste0(prefix, "processed/crp.RDS"))

l.list <- seq(0.5, 1.5, by = 0.1)

fcal$calpro_result <- log(fcal$calpro_result)
crp$crp_result <- log(crp$crp_result)

#############
### K = 2 ###
#############

stats.2<- data.frame(
  l = numeric(),
  loglik = numeric(),
  AIC = numeric(),
  BIC = numeric()
)
models.list.2 <- list()

for (i in seq_along(l.list)) {
  mod <- readRDS(paste0(
    prefix,
    "cache/grbf/k=2/grbf-",
    l.list[i],
    ".RDS"
  ))

  stats.2 <- rbind(stats.2, data.frame(
    l = l.list[i],
    loglik = mod$loglik,
    AIC = mod$AIC,
    BIC = mod$BIC
  ))
  models.list.2[[i]] <- mod
}

knitr::kable(stats.2, digits = 2)

if (!dir.exists("plots/grbf")) dir.create("plots/grbf")
if (!dir.exists("plots/grbf/k=2")) dir.create("plots/grbf/k=2")

for (i in seq_along(l.list)){
  temp.list <- list()
  temp.list[[6]] <- models.list.2[[i]]
  png(paste0("plots/grbf/k=2/", l.list[i], ".png"),
      width = 8,
      height = 7,
      units = "in",
      res = 300)
  spaghettiPlot(fcal,
                temp.list,
                G = 6,
                tmax = 7,
                sizes = TRUE,
                knots = TRUE,
                knot.type = "equal",
                n.knots = 2,
                grbf = TRUE,
                l = l.list[i],
                var.time = "calpro_time")
  dev.off()

  if (i == 1) {
    cairo_pdf(paste0("plots/grbf/k=2/", l.list[i], ".pdf"),
              width = 8,
              height = 7)
    spaghettiPlot(fcal,
                  temp.list,
                  G = 6,
                  tmax = 7,
                  ylim = "pred",
                  sizes = TRUE,
                  knots = TRUE,
                  knot.type = "equal",
                  n.knots = 2,
                  grbf = TRUE,
                  l = l.list[i],
                  var.time = "calpro_time")
    dev.off()
  }

  if (i == 8) {
    cairo_pdf(paste0("plots/grbf/k=2/", l.list[i], ".pdf"),
        width = 8,
        height = 7)
    spaghettiPlot(fcal,
                  temp.list,
                  G = 6,
                  tmax = 7,
                  sizes = TRUE,
                  knots = TRUE,
                  knot.type = "equal",
                  n.knots = 2,
                  grbf = TRUE,
                  l = l.list[i],
                  var.time = "calpro_time")
    dev.off()
  }
}

#############
### K = 3 ###
#############

stats.3 <- data.frame(
  l = numeric(),
  loglik = numeric(),
  AIC = numeric(),
  BIC = numeric()
)
models.list.3 <- list()

for (i in seq_along(l.list)) {
  mod <- readRDS(paste0(
    prefix,
    "cache/grbf/k=3/grbf-",
    l.list[i],
    ".RDS"
  ))

  stats.3 <- rbind(stats.3, data.frame(
    l = l.list[i],
    loglik = mod$loglik,
    AIC = mod$AIC,
    BIC = mod$BIC
  ))
  models.list.3[[i]] <- mod
}

knitr::kable(stats.3, digits = 2)

if (!dir.exists("plots/grbf/k=3")) dir.create("plots/grbf/k=3")

for (i in seq_along(l.list)){
  temp.list <- list()
  temp.list[[6]] <- models.list.3[[i]]
  png(paste0("plots/grbf/k=3/", l.list[i], ".png"),
      width = 8,
      height = 7,
      units = "in",
      res = 300)
  spaghettiPlot(fcal,
                temp.list,
                G = 6,
                tmax = 7,
                sizes = TRUE,
                knots = TRUE,
                knot.type = "equal",
                n.knots = 3,
                grbf = TRUE,
                l = l.list[i],
                var.time = "calpro_time")
  dev.off()

  if (i == 1) {
    cairo_pdf(paste0("plots/grbf/k=3/", l.list[i], ".pdf"),
              width = 8,
              height = 7)
    spaghettiPlot(fcal,
                  temp.list,
                  G = 6,
                  tmax = 7,
                  ylim = "pred",
                  sizes = TRUE,
                  knots = TRUE,
                  knot.type = "equal",
                  n.knots = 3,
                  grbf = TRUE,
                  l = l.list[i],
                  var.time = "calpro_time")
    dev.off()
  }

  if (i == 8) {
    cairo_pdf(paste0("plots/grbf/k=3/", l.list[i], ".pdf"),
        width = 8,
        height = 7)
    spaghettiPlot(fcal,
                  temp.list,
                  G = 6,
                  tmax = 7,
                  sizes = TRUE,
                  knots = TRUE,
                  knot.type = "equal",
                  n.knots = 3,
                  grbf = TRUE,
                  l = l.list[i],
                  var.time = "calpro_time")
    dev.off()
  }
}
