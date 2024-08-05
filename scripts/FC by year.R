library(tidyverse)
library(patchwork)
library(datefixR)


if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/Vallejo-predict/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}


fcal <- read.csv(paste0(prefix, "2022-12-07/fcal-cleaned.csv"))

# Extract from TRAK which also now introduces CRP.
labs <- read.csv(paste0(prefix, "2023-02-15/labs.csv"))
labs.fcal <- subset(labs, TEST == "f-Calprotectin-ALP")


fcal <- fcal[, c("ids", "calpro_date", "calpro_result")]
labs.fcal <- labs.fcal[, c("CHI", "COLLECTION_DATE", "TEST_DATA")]

# Collection dates include collection times which are not required. Discarding.
labs.fcal$COLLECTION_DATE <- stringr::str_split_fixed(labs.fcal$COLLECTION_DATE,
                                                      " ",
                                                      n = 2)[, 1]

colnames(labs.fcal) <- c("ids", "calpro_date", "calpro_result")
fcal <- rbind(fcal, labs.fcal)
fcal <- fix_date_df(fcal, "calpro_date")
fcal <- fcal %>% distinct(ids,
                          calpro_date,
                          calpro_result,
                          .keep_all = TRUE
)

p1 <- fcal %>%
  ggplot(aes(x = year(calpro_date))) +
  geom_bar(color = "#70ABAF", fill = "#99E1D9") +
  theme_minimal() +
  labs(x = "Year", y = "Faecal calprotectin test count")

crp <- subset(labs, TEST == "C-Reactive Prot")
crp$COLLECTION_DATE <- readr::parse_date(
  stringr::str_split_fixed(crp$COLLECTION_DATE, " ", n = 2)[, 1],
  "%d-%b-%Y"
)

crp <- crp %>%
  distinct(CHI,
           COLLECTION_DATE,
           TEST_DATA,
           .keep_all = TRUE
)

p2 <- crp %>%
  ggplot(aes(x = year(COLLECTION_DATE))) +
  geom_bar(color = "#773344", fill = "#E85F5C") +
  theme_minimal() +
  labs(x = "Year", y = "CRP test count")

p <- p1 / p2 + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

ggsave("plots/all-biomarkers.png",
       p,
       width = 12 * 2/3,
       height = 7,
       units = "in",
       dpi = 300)


ggsave("plots/all-biomarkers.pdf",
       p,
       width = 12 * 2/3,
       height = 7)
