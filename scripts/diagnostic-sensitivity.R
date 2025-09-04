## ----Package load-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| message: false
#| warning: false
#| cache: false
set.seed(123)
### Required packages
library(tidyverse) # ggplot2, dplyr, and magrittr
library(knitr) # Markdown utilities
library(pander) # Pretty markdown rendering
library(datefixR) # Standardising dates
library(lubridate) # Date handling


## ----Read files---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
if (file.exists("/.dockerenv")) { # Check if running in Docker
  # Assume igmm/Vallejo-predict/libdr/ is passed to the data volume
  prefix <- "data/"
} else {
  # Assume running outside of a Docker container and the IGC(/IGMM) datastore is
  # mounted at /Volumes
  prefix <- "/Volumes/igmm/cvallejo-predicct/libdr/"
}

fcal.pheno <- read.csv(paste0(prefix, "2024-10-03/fcal-cleaned.csv"))

# Extract from TRAK which also now introduces CRP.
labs <- read.csv(paste0(prefix, "2024-10-03/markers-cleaned.csv"))
fcal <- subset(labs, TEST == "f-Calprotectin-ALP")

# Add sex and diagnosis type from fcal.pheno
fcal <- fcal.pheno %>%
  distinct(ids, .keep_all = TRUE) %>%
  select(ids, sex, diagnosis, diagnosis_date) %>%
  merge(x = fcal, by = "ids", all.x = TRUE, all.y = FALSE)

crp <- subset(labs, TEST == "C-Reactive Prot")
updated <- read.csv(paste0(prefix, "2024-10-03/allPatientsNathanCleaned.csv"))
outcomes <- read.csv(paste0(prefix, "2024-10-03/cd-cleaned.csv"))

non.ibd <- read.csv(paste0 (prefix, "2024-10-24/non-ibd.csv"))


## ----Create subject dictionary------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ids <- unique(c(outcomes$ids, fcal$ids, updated$ids))
diagnosis <- character()
date.of.diag <- character()
for (id in ids) {
  if (id %in% outcomes$ids) {
    # Outcomes data only contains CD subjects
    diagnosis <- c(diagnosis, "Crohn's Disease")
    date.of.diag <- c(
      date.of.diag,
      subset(outcomes, ids == id)$diagnosisDate
    )
  } else if (id %in% updated$ids) {
    diagnosis <- c(diagnosis, subset(updated, ids == id)[1, "diagnosis"])
    date.of.diag <- c(
      date.of.diag,
      subset(updated, ids == id)[1, "diagnosisDate"]
    )
  } else if (id %in% fcal$ids) {
    diagnosis <- c(diagnosis, subset(fcal, ids == id)[1, "diagnosis"])
    date.of.diag <- c(
      date.of.diag,
      subset(fcal, ids == id)[1, "diagnosis_date"]
    )
  } else {
    diagnosis <- c(diagnosis, "Unknown")
    date.of.diag <- c(
      date.of.diag,
      "Unknown"
    )
  }
}

dict <- data.frame(
  ids = ids,
  diagnosis = diagnosis,
  date.of.diag = fix_date_char(date.of.diag)
)


rm(id, ids, diagnosis, date.of.diag) # clean up
dict <- fix_date_df(dict, "date.of.diag")


dict <- dict %>%
  subset(diagnosis != "Not IBD") %>%
  subset(!(ids %in% non.ibd$ids))


## ----Contingency table of IBD diagnosis pre and post processing---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-diag-post
#| tbl-cap: Contingency table showing mapping of IBD types to a standardised format
#| results: "hold"
dict$old <- dict$diagnosis
dict$diagnosis <- plyr::mapvalues(
  dict$diagnosis,
  from = c(
    "Crohns Disease",
    "Inflamatory Bowel Disease",
    "Inflamatory Bowel Disease - Unknown Subtype"
  ),
  to = c(
    "Crohn's Disease",
    "IBDU",
    "IBDU"
  )
)

## ----add sex to dict----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Merge with fcal to add sex information
dict <- fcal.pheno[, c("ids", "sex")] %>%
  distinct(ids,
    .keep_all = TRUE
  ) %>%
  merge(x = dict, by = "ids", all.x = TRUE, all.y = FALSE)

# Update NA sex if sex available from updated
dict <- merge(dict,
  updated[, c("ids", "sex")],
  by = "ids",
  all.x = TRUE,
  all.y = FALSE
)

for (i in seq_len(nrow(dict))) {
  if (is.na(dict[i, "sex.x"]) && !is.na(dict[i, "sex.y"])) {
    dict[i, "sex.x"] <- dict[i, "sex.y"]
  }
}

dict$sex <- dict$sex.x
dict$sex.x <- dict$sex.y <- NULL

# Add age at IBD diagnosis
updated <- fix_date_df(updated, "diagnosisDate")
updated$age <- with(updated, year(diagnosisDate) - dateOfBirth)
dict <- merge(dict,
  updated[, c("ids", "age")],
  by = "ids",
  all.x = TRUE,
  all.y = FALSE
)


## ----add date.of.death--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dict <- merge(dict,
  updated[, c("ids", "death")],
  by = "ids",
  all.x = TRUE,
  all.y = FALSE
) %>%
  rename(date.of.death = death)

dict <- fix_date_df(dict, "date.of.death")


## ----Remove NA date of diagnosis----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dict <- dict[!is.na(dict$date.of.diag), ]


## ----Apply date of diagnosis exclusion----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# no. subjects over upper bound
max_fup <- max(labs$COLLECTION_DATE)
max_fup <- as.Date(max_fup)
n.upper <- nrow(subset(dict, max_fup - date.of.diag < 5 * 365.25))

# no. subjects under lower bound
n.lower <- nrow(subset(dict, year(date.of.diag) < 2008))

# subset to subjects meeting the criteria.
dict <- subset(dict, max_fup - date.of.diag >= 5 * 365.25)
dict <- subset(dict, year(date.of.diag) >= 2008)


## ----Clean up excluded date of diagnosis counts-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| include: false
rm(n.upper, n.lower) # clean up


## ----FCAL merge---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fcal <- fcal[, c(
  "ids",
  "COLLECTION_DATE",
  "TEST_DATA",
  "sex",
  "diagnosis",
  "diagnosis_date"
)]

# Subset to only include those that passed the earlier inclusion/exclusion
fcal <- subset(fcal, ids %in% dict$ids)

# Collection dates include collection times which are not required. Discarding.

fcal$COLLECTION_DATE <- readr::parse_date(
  stringr::str_split_fixed(fcal$COLLECTION_DATE, " ", n = 2)[, 1],
  format = "%Y-%m-%d"
)

colnames(fcal)[1:3] <- c("ids", "calpro_date", "calpro_result")

fcal <- subset(fcal, ids %in% dict$ids)
fcal <- fcal %>%
  select(-diagnosis)

fcal <- merge(fcal,
  dict[, c("ids", "diagnosis", "date.of.diag")],
  by = "ids",
  all.x = TRUE
)


## ----FCAL censor mapping------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
## Some values cannot be directly coerced as numeric

fcal$calpro_result <- plyr::mapvalues(
  fcal$calpro_result,
  from = c(
    "<20",
    "<25",
    "<50",
    ">1250",
    ">2500",
    ">3000",
    ">6000"
  ),
  to = c(
    "20",
    "25",
    "50",
    "1250",
    "2500",
    "3000",
    "6000"
  )
)

# Here, values that cannot be converted into a numeric value will be excluded
# (they are converted to NA)
fcal$calpro_result <- as.numeric(fcal$calpro_result) # Remove error codes
fcal <- fcal[!is.na(fcal[, "calpro_result"]), ]

# Apply limits of detection
fcal[fcal[, "calpro_result"] < 20, "calpro_result"] <- 20
fcal[fcal[, "calpro_result"] > 1250, "calpro_result"] <- 1250

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
duplicated.ids <- c()

for (id in unique(fcal$ids)) {
  sub.fcal <- subset(fcal, ids == id) # Get FC data for a subject
  sub.fcal <- sub.fcal[order(sub.fcal$calpro_date), ] # Order by dates
  time.diff <- diff(sub.fcal$calpro_date) # Find time between ordered dates
  value.diff <- diff(sub.fcal$calpro_result) # Find difference in observed values
  # If two measurements are within 10 days of each other and have the same value
  if (any(time.diff <= 10 & value.diff == 0)) {
    duplicated.ids <- c(duplicated.ids, id)
    # Remove suspected duplicates (taking into account difference is lagged)
    sub.fcal <- sub.fcal[c(TRUE, !(time.diff <= 10 & value.diff == 0)), ]
    # Remove data for subject with duplicates
    fcal <- subset(fcal, ids != id)
    # Add non duplicated data back
    fcal <- rbind(fcal, sub.fcal)
  }
}

## ----FCAL time mapping--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-fcal-spag-pre
#| fig-cap: "Spaghetti plot of FC trajectories (preprocessed)"

# Dates have already been converted to Date class by fix_date_char() for dict
fcal$calpro_time <- as.numeric(fcal$calpro_date - fcal$date.of.diag) / 365.25

## ----remove prediag fcal------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fcal <- subset(fcal, calpro_time >= -0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##The following code is used to save the cleaned data generated so far.

diag.time <- c()
fc.ids <-  unique(fcal$ids)

for (id in fc.ids) {
  temp <- subset(fcal, ids == id)
  temp <- temp[order(temp$calpro_time), ]
  diag.time <- c(diag.time, temp[1, "calpro_time"])
}

fc.dist <- data.frame(ids = fc.ids, diagnostic = diag.time)

if (!dir.exists(paste0(prefix, "processed"))) {
  dir.create(paste0(prefix, "processed"))
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-fcal-spag-post
#| fig-cap: "Spaghetti plot of FC trajectories (processed)"
# Retime so that t_0 = 0.
for (id in unique(fcal$ids)) {
  temp <- subset(fcal, ids == id)
  if (any(temp$calpro_time < 0)) {
    add <- sort(temp$calpro_time)[1]
    fcal[fcal[, "ids"] == id, "calpro_time"] <-
      fcal[fcal[, "ids"] == id, "calpro_time"] + abs(add)
  }
}


## ----remove fcal after 7 years------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fcal <- subset(fcal, calpro_time <= 7)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countsDF <- fcal %>%
  group_by(ids) %>%
  summarise(
    n.total = n(),
    censored.left = sum(calpro_result == 20),
    censored.right = sum(calpro_result == 1250),
    n.noncensored = n.total - censored.left - censored.right,
    n.negtime.nondiag = sum(calpro_time != 0 & calpro_date - date.of.diag < 0),
    followup = max(calpro_time)
  )

## ----FCAL frequency-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fcal <- fcal %>%
  subset(ids %in% countsDF$ids[countsDF$n.noncensored >= 3])


## ----Month of diagnosis (postprocessed)---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-diag-month-redux
#| fig-cap: "Bar plot of month of diagnosis"
dict.temp <- subset(dict, ids %in% unique(fcal$ids))

## ----crp preprocess-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crp <- crp[, c("ids", "COLLECTION_DATE", "TEST_DATA")] %>%
  subset(ids %in% dict$ids)

# Collection dates include collection times which are not required. Discarding.
crp$COLLECTION_DATE <- readr::parse_date(
  stringr::str_split_fixed(crp$COLLECTION_DATE, " ", n = 2)[, 1],
  format = "%Y-%m-%d"
)

colnames(crp) <- c("ids", "crp_date", "crp_result")

crp <- merge(crp,
  dict[, c("ids", "diagnosis")],
  by = "ids",
  all.x = TRUE
)


## ----crp censor mapping-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| warning: false
crp$crp_result <- as.numeric(
  plyr::mapvalues(
    crp$crp_result,
    from = c(
      "<0.2", "<1", "<1.", "<1.0", "<2", "<3", "<3.0", "<5",
      ">90", ">320"
    ),
    to = c(1, 1, 1, 1, 2, 3, 3, 5, 90, 320)
  )
)

# Lower-bound censoring
crp <- crp %>%
  mutate(crp_result = if_else(crp_result < 1, 1, crp_result))

# Remove error test results
crp <- crp[!is.na(crp[, "crp_result"]), ]
# Map numerical <1 test results (e.g 0.2) to 1.


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crp <- crp %>% distinct(ids,
  crp_date,
  crp_result,
  .keep_all = TRUE
)


## ----CRP time mapping---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-crp-spag-pre
#| fig-cap: "Spaghetti plot of CRP trajectories (preprocessed)"

crp <- merge(crp, dict[, c("ids", "date.of.diag")],
  by = "ids", all.x = TRUE, all.y = FALSE
)

# Dates have already been converted to Date class by fix_date_char() for dict
crp$crp_time <- as.numeric(crp$crp_date - crp$date.of.diag) / 365.25


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crp <- subset(crp, crp_time >= -0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diag.time <- c()
crp.ids <-  unique(crp$ids)

for (id in crp.ids) {
  temp <- subset(crp, ids == id)
  temp <- temp[order(temp$crp_time), ]
  diag.time <- c(diag.time, temp[1, "crp_time"])
}




## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Retime so that t_0 = 0.
for (id in unique(crp$ids)) {
  temp <- subset(crp, ids == id)
  if (any(temp$crp_time < 0)) {
    add <- sort(temp$crp_time)[1]
    crp[crp[, "ids"] == id, "crp_time"] <-
      crp[crp[, "ids"] == id, "crp_time"] + abs(add)
  }
}


## ----remove crp after 7 years-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crp <- subset(crp, crp_time <= 7)



## ----crp additional preproc---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
id.list <- unique(crp$ids)
crp.ma <- matrix(NA, nrow = length(id.list), ncol = 7)

for (i in seq_along(id.list)) {
  subject_data <- subset(crp, ids == id.list[i])
  for (j in seq(0, 6)) {
    if (j == 6) {
      sub.obs <- subset(
        subject_data,
        crp_time >= j - 0.5 & crp_time <= j + 1
      )
    } else {
      sub.obs <- subset(
        subject_data,
        crp_time >= j - 0.5 & crp_time < j + 0.5
      )
    }
    if (nrow(sub.obs) > 0) {
      crp.ma[i, j + 1] <- median(sub.obs$crp_result)
    }
  }
}
rownames(crp.ma) <- id.list

# Convert back to a long-format dataset and rename columns
crp.ma <- reshape2::melt(t(crp.ma), id.vars = row.names(crp.ma), na.rm = TRUE)
colnames(crp.ma) <- c("crp_time", "ids", "crp_result")
crp.ma <- crp.ma[, c(2, 3, 1)]

# Reset the time to start at zero
crp.ma$crp_time <- crp.ma$crp_time - 1

# Take into account uneven spacing at start and end
crp.ma$crp_time <- plyr::mapvalues(crp.ma$crp_time,
  from = c(0, 6),
  to = c(0.25, 6.25)
)

# Add diagnosis type back
crp.ma <- merge(crp.ma, dict[, -3], by = "ids", all.x = TRUE, all.y = FALSE)


## ----crp followup pre exclusions post processed-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-crp-follow-up-postproc
#| fig-cap: "(A) Histogram of the number of CRP measurements per subject. (B) Histogram of CRP follow-up per subject (before exclusions). (C) Scatterplot with number of CRP measurements vs follow-up."
#| fig-width: 12
#| column: body-outset

countsDF <- crp.ma %>%
  group_by(ids) %>%
  summarise(
    n.total = n(),
    censored.left = sum(crp_result == 0), # 0 because it was log transformed
    n.noncensored = n.total - censored.left,
    followup = max(crp_time),
    crp.var = var(crp_result, na.rm = TRUE)
  )


## ----CRP frequency------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crp.ma.0 <- crp.ma %>%
  subset(ids %in% countsDF$ids[countsDF$n.total >= 3 & countsDF$crp.var == 0])


crp.ma <- crp.ma %>%
  subset(ids %in% countsDF$ids[countsDF$n.total >= 3 & countsDF$crp.var != 0])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
if (!dir.exists(paste0(prefix, "processed"))) {
  dir.create(paste0(prefix, "processed"))
}

fcal$calpro_result <- log(fcal$calpro_result)
crp.ma$crp_result <- log(crp.ma$crp_result)


saveRDS(dict, paste0(prefix, "processed/dict-initial-nodiag.RDS"))
saveRDS(fcal, paste0(prefix, "processed/fcal-nodiag.RDS"))
saveRDS(crp, paste0(prefix, "processed/crp-nodiag.RDS"))
saveRDS(crp.ma, paste0(prefix, "processed/median-crp-nodiag.RDS"))
