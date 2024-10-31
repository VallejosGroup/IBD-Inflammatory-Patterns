# Legacy code deleted from the associations.qmd file

##### Montreal classification of age for Crohn's disease

Unlike ulcerative colitis, age at diagnosis is part of the Montreal
classification for Crohn's disease.  As such, we will explore potential
associations between Montreal age classification and cluster membership for
subjects diagnosed with Crohn's disease. The Montreal age categories are as
follows:

  * A1: $\leq$ 16 years
* A2: 17—40 years
* A3: $>$ 40 years

Montreal classification of age for Crohn's disease was not found to be
significantly associated with cluster membership.

```{R}
ageLabels <- c("\u226416", "17—40", "> 40")

myDF.fc.cd <- subset(myDF.fc, diagnosis == "Crohn's Disease")

myDF.fc.cd$ageCat <- cut(myDF.fc.cd$age,
  breaks = c(0, 16, 40, Inf),
  labels = ageLabels,
  include.lowest = TRUE,
  right = FALSE
)

myDF.fc.cd %>%
  filter(probmax_combined > 0.5) %>%
  with(fisher.test(class_combined,
                   ageCat,
                   workspace = 2000000,
                   simulate.p.value = TRUE)) %>%
  pander()
```

```{R}
myDF.fc.cd %>%
  filter(probmax_combined > 0.5) %>%
  mlrPlot(var = "ageCat")
```

#### Time to first line AT usage

```{r}
myDF.fc <- myDF.fc %>%
  mutate(AT_line_1_censor = if_else(AT == 0, 7, as.numeric(AT_line_1))) %>%
  mutate(AT_line_1_censor = if_else(AT_line_1_censor > 7, 7, AT_line_1_censor),
         AT_censor = if_else(AT_line_1_censor == 7, 0, AT))

p_tte <- survfit(Surv(AT_line_1_censor, AT_censor) ~ class_order, data = myDF.fc) %>%
  ggsurvplot(pval = TRUE) +
  ggtitle("CD + UC + IBDU")

ATcum<- cmprsk::cuminc(
  ftime = myDF.fc$AT_line_1_censor,
  fstatus = myDF.fc$AT_censor,
  group = myDF.fc$class_order)

ATcum_cd <- cmprsk::cuminc(
  ftime = myDF.fc$AT_line_1_censor,
  fstatus = myDF.fc$AT_censor,
  group = myDF.fc$class_order,
  subset = ifelse(myDF.fc$diagnosis == "Crohn's Disease", TRUE, FALSE))

ATcum_uc <- cmprsk::cuminc(
  ftime = myDF.fc$AT_line_1_censor,
  fstatus = myDF.fc$AT_censor,
  group = myDF.fc$class_order,
  subset = ifelse(myDF.fc$diagnosis == "Ulcerative Colitis", TRUE, FALSE))

  ggcompetingrisks(fit) + facet_wrap(ncol = 2)
  ggtitle("CD + UC + IBDU")

  ggcompetingrisks(ATcum_uc)
```


#### All lines

```{R}
fc.cd.at <-
fc.cd.at$Line <- as.factor(str_replace(fc.cd.at$Line, "AT_line_", ""))

myDF.fc.cd <- subset(myDF.fc, diagnosis == "Crohn's Disease")



scaling <- table(fc.cd.at$class)/table(myDF.fc.cd$class)


plots <- list()




for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")

  scaler <- scaling[[g]]

  temp <- fc.cd.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)

  dens <- density(temp$value, kernel = "gaussian", from = 0, to = 7)
  dens$y <- dens$y / max(dens$y) # Scale density to 1
  dens <- data.frame(x = dens$x, y = dens$y)
  dens$y <- dens$y * scaler * 4

  plots[[g]] <- dens %>%
    ggplot() +
    geom_area(aes(x = x,
                  y = y),
                 fill = "#AB87FF",
color =  "#7903F0",
linewidth = 1.2,
lty = 3,
alpha = 0.7) +
  geom_line(aes(x = time, y = mean),
            prediction,
            color = "red",
            linewidth = 1.2) +
  geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
  scale_y_continuous(
    name = "Log (FC (\u03BCg/g))",
    limits = c(0, ylimit),
    sec.axis = sec_axis(transform = ~./ylimit,
                        name = "Advanced therapy density")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
  xlab("Time (years)") +
  ggtitle(paste0("FC", title.mapping[g]))
}

mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-cd-at.png", p, width = 10, height = 14, units = "in")
p
```


```{R}
fc.cd.at <- reshape2::melt(dict.fc.cd,
                           id.vars = "ids",
                           measure.vars = paste0("AT_line_", seq(1, 8)),
                           variable.name = "Line") %>%
  drop_na(value)
fc.cd.at$Line <- as.factor(str_replace(fc.cd.at$Line, "AT_line_", ""))

myDF.fc.cd <- subset(myDF.fc, diagnosis == "Crohn's Disease")


fc.cd.at <- merge(fc.cd.at,
                  myDF.fc[, c("ids", "class")],
                  by = "ids",
                  all.x = TRUE,
                  all.y = FALSE)


time.pred <- seq(0, 7, by = 0.01)

pred.fc.df <- data.frame(
  calpro_time = c(time.pred, time.pred),
  diagnosis = c(
    rep("Crohn's Disease", length(time.pred)),
    rep("Ulcerative Colitis", length(time.pred))
  )
)
pred.fc.df.update <- lcmm::predictY(model.fc,
                                    pred.fc.df,
                                    var.time = "calpro_time",
                                    draws = TRUE
)$pred

pred <- predictY(model.fc, pred.fc.df, var.time = "calpro_time", draws = TRUE)$pred

pred <- as.data.frame(pred[1:length(time.pred), ])
pred$time <- time.pred

plots <- list()

ylimit <- log(2500)
title.mapping <- c(7, 6, 4, 8, 1, 5, 2, 3)


for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")


  temp <- fc.cd.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)


  temp %>%
    summarise()

  plots[[g]] <- temp %>%
    ggplot() +
    geom_histogram(aes(x = value, y = after_stat(count)),
                   fill = "#AB87FF",
                   color =  "#7903F0") +
    geom_line(aes(x = time, y = mean),
              prediction,
              color = "red",
              linewidth = 1.2) +
    geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
    scale_y_continuous(
      name = "Log (FC (\u03BCg/g))",
      limits = c(0, ylimit),
      sec.axis = sec_axis(transform = ~./ylimit,
                          name = "Advanced therapy density")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
    xlab("Time (years)") +
    ggtitle(paste0("FC", title.mapping[g]))
}

mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-cd-at-hist.png", p, width = 10, height = 14, units = "in")
p
```

#### First-line only

```{R}
myDF.fc.cd <- subset(myDF.fc, diagnosis == "Crohn's Disease")
fc.cd.at <- fc.cd.at %>%
  filter(Line == 1)

scaling <- table(fc.cd.at$class)/table(myDF.fc.cd$class)

time.pred <- seq(0, 7, by = 0.01)

pred.fc.df <- data.frame(
  calpro_time = c(time.pred, time.pred),
  diagnosis = c(
    rep("Crohn's Disease", length(time.pred)),
    rep("Ulcerative Colitis", length(time.pred))
  )
)
pred.fc.df.update <- lcmm::predictY(model.fc,
                                    pred.fc.df,
                                    var.time = "calpro_time",
                                    draws = TRUE
)$pred

pred <- predictY(model.fc, pred.fc.df, var.time = "calpro_time", draws = TRUE)$pred

pred <- as.data.frame(pred[1:length(time.pred), ])
pred$time <- time.pred

plots <- list()

ylimit <- log(2500)
title.mapping <- c(7, 6, 4, 8, 1, 5, 2, 3)


for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")

  scaler <- scaling[[g]]

  temp <- fc.cd.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)

  dens <- density(temp$value, kernel = "gaussian", from = 0, to = 7)
  dens$y <- dens$y / max(dens$y) # Scale density to 1
  dens <- data.frame(x = dens$x, y = dens$y)
  dens$y <- dens$y * scaler * 4

  plots[[g]] <- dens %>%
    ggplot() +
    geom_area(aes(x = x,
                  y = y),
              fill = "#AB87FF",
              color =  "#7903F0",
              linewidth = 1.2,
              lty = 3,
              alpha = 0.7) +
    geom_line(aes(x = time, y = mean),
              prediction,
              color = "red",
              linewidth = 1.2) +
    geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
    scale_y_continuous(
      name = "Log (FC (\u03BCg/g))",
      limits = c(0, ylimit),
      sec.axis = sec_axis(transform = ~./ylimit,
                          name = "Advanced therapy density")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
    xlab("Time (years)") +
    ggtitle(paste0("FC", title.mapping[g]))
}

mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-cd-at-firstline.png", p, width = 10, height = 14, units = "in")
p
```


```{R}
dict.fc.cd <- subset(myDF.fc, diagnosis == "Crohn's Disease" & ids %in% fcal$ids)

#Apply censoring
dict.fc.cd <- dict.fc.cd %>%
  mutate(AT_line_1  = if_else(AT == 0, 7, as.numeric(AT_line_1))) %>%
  mutate(AT = if_else(AT_line_1 > 7, 0, 1)) %>%
  mutate(AT_line_1 = if_else(AT_line_1  > 7, 7, AT_line_1),
         AT = if_else(AT_line_1 == 7, 0, AT))
survfit(Surv(AT_line_1, AT) ~ class_order, data = dict.fc.cd) %>%
  ggsurvplot(pval = TRUE)
```

```{R}
#| echo: false
png("plots/at-survival-fc-ci.png",
    width = 7,
    height = 5,
    units = "in",
    res = 300
)
survfit(Surv(AT_line_1, AT) ~ class_order, data = dict.fc.cd) %>%
  ggsurvplot(pval = TRUE, conf.int = TRUE)
invisible(dev.off())
```

### Advanced therapy in UC

#### All lines

```{R}
dict.fc.uc <- dict %>%
  filter(ids %in% fcal$ids) %>%
  filter(diagnosis == "Ulcerative Colitis")
```

```{R}
fc.uc.at <- reshape2::melt(dict.fc.uc,
                           id.vars = "ids",
                           measure.vars = paste0("AT_line_", seq(1, 8)),
                           variable.name = "Line") %>%
  drop_na(value) %>%
  mutate(value = as.numeric(value))
fc.uc.at$Line <- as.factor(str_replace(fc.uc.at$Line, "AT_line_", ""))

myDF.fc.uc <- subset(myDF.fc, diagnosis == "Ulcerative Colitis")


fc.uc.at <- merge(fc.uc.at,
                  myDF.fc[, c("ids", "class")],
                  by = "ids",
                  all.x = TRUE,
                  all.y = FALSE)

scaling <- table(fc.uc.at$class)/table(myDF.fc.uc$class)

time.pred <- seq(0, 7, by = 0.01)

pred.fc.df <- data.frame(
  calpro_time = c(time.pred, time.pred),
  diagnosis = c(
    rep("Crohn's Disease", length(time.pred)),
    rep("Ulcerative Colitis", length(time.pred))
  )
)
pred.fc.df.update <- lcmm::predictY(model.fc,
                                    pred.fc.df,
                                    var.time = "calpro_time",
                                    draws = TRUE
)$pred

pred <- predictY(model.fc, pred.fc.df, var.time = "calpro_time", draws = TRUE)$pred

pred <- as.data.frame(pred[1:length(time.pred), ])
pred$time <- time.pred

plots <- list()

ylimit <- log(2500)
title.mapping <- c(7, 6, 4, 8, 1, 5, 2, 3)


for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")

  scaler <- scaling[[g]]

  temp <- fc.uc.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)

  dens <- density(temp$value, kernel = "gaussian", from = 0, to = 7)
  dens$y <- dens$y / max(dens$y) # Scale density to 1
  dens <- data.frame(x = dens$x, y = dens$y)
  dens$y <- dens$y * scaler * 4

  plots[[g]] <- dens %>%
    ggplot() +
    geom_area(aes(x = x,
                  y = y),
              fill = "#AB87FF",
              color =  "#7903F0",
              linewidth = 1.2,
              lty = 3,
              alpha = 0.7) +
    geom_line(aes(x = time, y = mean),
              prediction,
              color = "red",
              linewidth = 1.2) +
    geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
    scale_y_continuous(
      name = "Log (FC (\u03BCg/g))",
      limits = c(0, ylimit),
      sec.axis = sec_axis(transform = ~./ylimit,
                          name = "Advanced therapy density")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
    xlab("Time (years)") +
    ggtitle(paste0("FC", title.mapping[g]))
}


mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-uc-at.png", p, width = 10, height = 14, units = "in")
p
```


#### First-line only

```{R}
myDF.fc.uc <- subset(myDF.fc, diagnosis == "Ulcerative Colitis")
fc.uc.at <- fc.uc.at %>%
  filter(Line == 1)

scaling <- table(fc.uc.at$class)/table(myDF.fc.uc$class)

time.pred <- seq(0, 7, by = 0.01)

pred.fc.df <- data.frame(
  calpro_time = c(time.pred, time.pred),
  diagnosis = c(
    rep("Crohn's Disease", length(time.pred)),
    rep("Ulcerative Colitis", length(time.pred))
  )
)
pred.fc.df.update <- lcmm::predictY(model.fc,
                                    pred.fc.df,
                                    var.time = "calpro_time",
                                    draws = TRUE
)$pred

pred <- predictY(model.fc, pred.fc.df, var.time = "calpro_time", draws = TRUE)$pred

pred <- as.data.frame(pred[1:length(time.pred), ])
pred$time <- time.pred

plots <- list()

ylimit <- log(2500)
title.mapping <- c(7, 6, 4, 8, 1, 5, 2, 3)


for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")

  scaler <- scaling[[g]]

  temp <- fc.uc.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)

  dens <- density(temp$value, kernel = "gaussian", from = 0, to = 7)
  dens$y <- dens$y / max(dens$y) # Scale density to 1
  dens <- data.frame(x = dens$x, y = dens$y)
  dens$y <- dens$y * scaler * 4

  plots[[g]] <- dens %>%
    ggplot() +
    geom_area(aes(x = x,
                  y = y),
              fill = "#AB87FF",
              color =  "#7903F0",
              linewidth = 1.2,
              lty = 3,
              alpha = 0.7) +
    geom_line(aes(x = time, y = mean),
              prediction,
              color = "red",
              linewidth = 1.2) +
    geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
    scale_y_continuous(
      name = "Log (FC (\u03BCg/g))",
      limits = c(0, ylimit),
      sec.axis = sec_axis(transform = ~./ylimit,
                          name = "Advanced therapy density")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
    xlab("Time (years)") +
    ggtitle(paste0("FC", title.mapping[g]))
}


mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-uc-at-firstline.png", p, width = 10, height = 14, units = "in")
p
```

```{R}
dict.fc.uc <- subset(myDF.fc, diagnosis == "Ulcerative Colitis" & ids %in% fcal$ids)

#Apply censoring
dict.fc.uc <- dict.fc.uc %>%
  mutate(AT_line_1  = if_else(AT == 0, 7, as.numeric(AT_line_1 ))) %>%
  mutate(AT = if_else(AT_line_1 > 7, 0, 1)) %>%
  mutate(AT_line_1 = if_else(AT_line_1  > 7, 7, AT_line_1),
         AT = if_else(AT_line_1 == 7, 0, AT))
survfit(Surv(AT_line_1, AT) ~ class_order, data = dict.fc.uc) %>%
  ggsurvplot(pval = TRUE)
```

### Advanced therapy for CD and UC

#### All lines

```{R}

```

```{R}
dict.fc <- dict %>%
  filter(ids %in% fcal$ids) %>%
  filter(diagnosis == "Crohn's Disease" | diagnosis == "Ulcerative Colitis")
```

```{R}
fc.at <- reshape2::melt(dict.fc,
                        id.vars = "ids",
                        measure.vars = paste0("AT_line_", seq(1, 8)),
                        variable.name = "Line") %>%
  drop_na(value) %>%
  mutate(value = as.numeric(value))
fc.at$Line <- as.factor(str_replace(fc.at$Line, "AT_line_", ""))

myDF.fc.temp <- subset(myDF.fc, diagnosis == "Crohn's Disease" |
                         diagnosis == "Ulcerative Colitis")


fc.at <- merge(fc.at,
               myDF.fc[, c("ids", "class")],
               by = "ids",
               all.x = TRUE,
               all.y = FALSE)

scaling <- table(fc.at$class)/table(myDF.fc.temp$class)

time.pred <- seq(0, 7, by = 0.01)

pred.fc.df <- data.frame(
  calpro_time = c(time.pred, time.pred),
  diagnosis = c(
    rep("Crohn's Disease", length(time.pred)),
    rep("Ulcerative Colitis", length(time.pred))
  )
)
pred.fc.df.update <- lcmm::predictY(model.fc,
                                    pred.fc.df,
                                    var.time = "calpro_time",
                                    draws = TRUE
)$pred

pred <- predictY(model.fc, pred.fc.df, var.time = "calpro_time", draws = TRUE)$pred

pred <- as.data.frame(pred[1:length(time.pred), ])
pred$time <- time.pred

plots <- list()

ylimit <- log(2500)
title.mapping <- c(7, 6, 4, 8, 1, 5, 2, 3)


for (g in 1:8) {
  prediction <- pred[, c(g, g + 8, g + 16, 25)]
  names(prediction) <- c("mean", "lower", "upper", "time")

  scaler <- scaling[[g]]

  temp <- fc.at %>%
    filter(class == paste0(g)) %>%
    filter(value < 7)

  dens <- density(temp$value, kernel = "gaussian", from = 0, to = 7)
  dens$y <- dens$y / max(dens$y) # Scale density to 1
  dens <- data.frame(x = dens$x, y = dens$y)
  dens$y <- dens$y * scaler * 4

  plots[[g]] <- dens %>%
    ggplot() +
    geom_area(aes(x = x,
                  y = y),
              fill = "#AB87FF",
              color =  "#7903F0",
              linewidth = 1.2,
              lty = 3,
              alpha = 0.7) +
    geom_line(aes(x = time, y = mean),
              prediction,
              color = "red",
              linewidth = 1.2) +
    geom_hline(yintercept = log(250), lty = 2, color =  "#007add") +
    scale_y_continuous(
      name = "Log (FC (\u03BCg/g))",
      limits = c(0, ylimit),
      sec.axis = sec_axis(transform = ~./ylimit,
                          name = "Advanced therapy density")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))  +
    xlab("Time (years)") +
    ggtitle(paste0("FC", title.mapping[g], "CD: "))
}


mapping <- c(5, 7, 8, 3, 6, 2, 1, 4)

for (i in 1:8) {
  if (i == 1) {
    p <- plots[[mapping[i]]]
  } else {
    p <- p + plots[[mapping[i]]]
  }
}

p <- p + plot_layout(ncol = 2)
ggsave("plots/fc-at.png", p, width = 10, height = 14, units = "in")
p
```






```{R}

dat <- data.frame(Cluster = character(), CD = numeric(), UC = numeric())


for (g in 1:8) {
  total <- myDF.fc %>%
    mutate(AT_line_1 = if_else(AT == 0, 7, as.numeric(AT_line_1))) %>%
    mutate(AT = if_else(AT_line_1 >= 7, 0, AT)) %>%
    mutate(AT_line_1  = if_else(AT_line_1  > 7, 7, AT_line_1)) %>%
    mutate(AT_line_1 = if_else(AT_line_1 < 0, 0, AT_line_1)) %>%
    filter(class_order == paste0("FC", g)) %>%
    summarise(CD = sum(diagnosis == "Crohn's Disease"),
              UC = sum(diagnosis == "Ulcerative Colitis"))

  therapy <- myDF.fc %>%
    mutate(AT_line_1 = if_else(AT == 0, 7, as.numeric(AT_line_1))) %>%
    mutate(AT = if_else(AT_line_1 >= 7, 0, AT)) %>%
    mutate(AT_line_1  = if_else(AT_line_1  > 7, 7, AT_line_1)) %>%
    mutate(AT_line_1 = if_else(AT_line_1 < 0, 0, AT_line_1)) %>%
    filter(class_order == paste0("FC", g)) %>%
    filter(AT == 1) %>%
    summarise(CD = sum(diagnosis == "Crohn's Disease"),
              UC = sum(diagnosis == "Ulcerative Colitis"))

  dat <- rbind(dat,
               data.frame(Cluster = paste0("FC", g),
                          CD = (therapy$CD/total$CD) * 100,
                          UC = (therapy$UC/total$UC) * 100))
}
```
