<!--- CAV: this code is not needed as we set the max follow-up to 7;
save as a backup elsewhere

There are `r length(unique(fcal$ids))` subjects who have at least three
FC measurements available (before [reducing the maximum
                                   followup](#maximum-followup)).

### Maximum followup {#maximum-followup}

A maximum follow-up should be specified in the interest of constructing
robust models which are not heavily influenced by observations at the
end of follow-up where observations are more sparse. We also ensure a
subject is only included if they have at least three FC measurements
during the restricted follow-up.

The decision for a specific cut-off is based upon two tables.
@tbl-follow-up explores how increasing the follow-up changes the mean,
median, and interquartile range of the number of observations per
subject. @tbl-count-fcal investigates how many subjects have at least
one FCAL after a set threshold.

```{R}
#| label: tbl-follow-up
#| tbl-cap: Statistics for number of observations across a range of maximum follow-up
years <- seq(1, 15, by = 1)
mean.n <- c()
median.n <- c()
IQR.n <- c()
freq <- c()

for (year in years) {
# restrict to measurements within threshold
temp <- subset(fcal, calpro_time <= year)
# restrict to subjects with at least three measurements within threshold
temp <- subset(temp, ids %in% as.numeric(names(table(temp$ids)[table(temp$ids) >= 3])))
counts <- table(temp$ids)
mean.n <- c(mean.n, round(mean(counts), 2))
median.n <- c(median.n, median(counts))
IQR.n <- c(IQR.n, paste(quantile(counts)[c(2, 4)], collapse = "-"))
freq <- c(freq, length(names(counts)))
}

knitr::kable(
data.frame(
years = years,
freq = freq,
mean.n = mean.n,
median.n = median.n,
IQR.n = IQR.n
)[5:15, ],
col.names = c(
"Year cut-off",
"Number of subjects",
"Mean",
"Median",
"IQR"
)
)
```

```{R}
#| label: tbl-count-fcal
#| tbl-cap: "How many subjects have an FC available after a given number of years?"
count <- numeric()
for (year in years) {
count <- c(count, length(unique(subset(fcal, calpro_time > year)$ids)))
}

count.tab <- data.frame(
years = years,
count = count
)
kable(count.tab, col.names = c("Years", "Number of subjects"))
```

From the above table, ten years of maximum follow-up appears to be the
most appropriate. However, we instead elect to use seven years of
maximum follow-up in order to [be consistent with
CRP](#maximum-followup-1)

```{R}
fcal <- subset(fcal, calpro_time <= 7)
# restrict to subjects with at least three measurements within threshold
fcal <- subset(
  fcal,
  ids %in% as.numeric(
    names(
      table(fcal$ids)[table(fcal$ids) >= 3]
    )
  )
)

n.4 <- length(
  unique(
    subset(
      fcal,
      ids %in% as.numeric(
        names(
          table(fcal$ids)[table(fcal$ids) >= 4]
        )
      )
    )$ids
  )
)


n.5 <- length(
  unique(
    subset(
      fcal,
      ids %in% as.numeric(
        names(
          table(fcal$ids)[table(fcal$ids) >= 5]
        )
      )
    )$ids
  )
)
```

### Censored observations

```{R Generate FCAL counts}
id.vector <- numeric()
count <- numeric()
censored.left <- numeric()
censored.right <- numeric()

for (id in unique(fcal$ids)) {
  fcal.subject <- subset(fcal, ids == id)
  id.vector <- c(id.vector, id)
  count <- c(count, nrow(fcal.subject))
  censored.left <- c(censored.left, sum(fcal.subject$calpro_result == 20))
  censored.right <- c(censored.right, sum(fcal.subject$calpro_result == 1250))
}

countsDF <- tibble(
  ids = id.vector,
  count = count,
  censored.left = censored.left,
  censored.right = censored.right
)
```

We now explore censored FC observations and begin by considering FC
results which have been left-censored ($\leq 20 \mu g/g$). From
@fig-left-censor-fcal, we can clearly see there are subjects with
entirely left-censored FC observations (n =
                                          `r nrow(subset(countsDF, censored.left/count ==1))`).

```{R}
#| label: fig-left-censor-fcal
#| fig-cap: "Left-censored observations per subject"
#  Hold results for no. of measurements and no. of censored obs per subject
countsDF %>%
  ggplot(aes(x = censored.left / count, y = count)) +
  geom_point(alpha = 0.5, size = 0.9, color = "#70A288") +
  ylab("Total number of observations") +
  xlab("Proportion of left-censored observations") +
  theme_minimal() +
  theme(legend.position = "none")
```

Looking at right-censored observations (@fig-right-censor-fcal), we see
there are fewer subjects with a high proportion (\>50% of censored
                                                 observations). Nevertheless, there are still subjects with only right
censored observations (n =
                         `r nrow(subset(countsDF, censored.right/count ==1))`).

```{R}
#| label: fig-right-censor-fcal
#| fig-cap: "Right-censored observations per subject"
countsDF %>%
  ggplot(aes(
    x = censored.right / count, # Proportion censored
    y = count
  )) + # Total censored
  # Add jitter to better show distribution
  geom_point(alpha = 0.5, size = 0.9, color = "#D5896F") +
  ylab("Total number of observations") +
  xlab("Proportion of right-censored observations") +
  theme_minimal() +
  theme(legend.position = "none")
```

In line with our inclusion criteria, we exclude subjects with fewer than
three non-censored observations.

```{R}
keep <- countsDF[with(countsDF, count - (censored.left + censored.right) >= 3), ]
fcal <- subset(fcal, ids %in% keep$ids)
```

This results in a cohort size of `r length(unique(fcal$ids))`
(`r nrow(countsDF) - nrow(keep)` subjects removed).

--->
