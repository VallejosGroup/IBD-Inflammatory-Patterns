hmm <- c()
temp <- fcal.original
temp <- fix_date_df(temp, c("diagnosis_date", "calpro_date"))
temp <- subset(temp, !is.na(diagnosis_date))
temp <- subset(temp, !is.na(calpro_date))




for (subject in unique(temp$ids)) {
  temp.2 <- subset(temp, ids == subject)
  if (any(abs(temp.2$diagnosis_date - temp.2$calpro_date) < 60)) {
    hmm <- c(hmm, subject)
  }
}

temp.3 <- subset(temp, ids %in% hmm)

counts <- data.frame(ids = unique(temp.3$ids), freq = 0)
for (i in 1:nrow(counts)) {
  counts[i, "freq"] <- nrow(subset(temp.3, ids == counts[i ,"ids"]))
}
