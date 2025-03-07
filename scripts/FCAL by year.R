fcal <- fcal %>%
  select(COLLECTION_DATE, TEST_DATA, ids)
fcal$COLLECTION_DATE <- as.Date(fcal$COLLECTION_DATE)


fcal.pheno <- fcal.pheno %>%
  select(calpro_date, calpro_result, ids)

names(fcal.pheno) <- c("COLLECTION_DATE", "TEST_DATA", "ids")



fcal <- rbind(fcal, fcal.pheno) %>%
  distinct() %>%
  fix_date_df("COLLECTION_DATE")

data.frame(year.of.test = year(fcal$COLLECTION_DATE)) %>%
  filter(year.of.test <= 2023) %>%
  ggplot(aes(x = year.of.test)) +
  geom_bar() + theme_minimal() +labs(y = "Number of Faecal calprotectin tests")
