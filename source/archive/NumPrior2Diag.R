<!--- CAV: remove this code, but keep as a back-up elsewhere

### Number of FCAL measurements prior to diagnosis

We consider measurements taken more than 90 days before diagnosis. As it can be seen in

```{r fcal before diagnosis}
fcal.sorted <- fcal %>%
  mutate(days.from.diag = calpro_date - date.of.diag) %>%
  arrange(ids, calpro_date)


mydf <- fcal.sorted %>%
  group_by(ids) %>%
  summarise(n.before.diag = sum(days.from.diag < -3*30),
            n.high.before.diag = sum(
              days.from.diag < -3*30 & calpro_result > 250
            )
  ) %>%
  arrange(desc(n.before.diag))

mydf %>%
  ggplot(aes(x = n.before.diag)) +
  geom_histogram(fill = "#887C9B") +
  theme_minimal() +
  ggtitle("Pre-diagnosis (> 90 days) FCAL observations per subject") +
  xlab("Number of observations per subject") +
  ylab("Frequency")

mydf %>%
  ggplot(aes(x = n.high.before.diag)) +
  geom_histogram(fill = "#887C9B") +
  theme_minimal() +
  ggtitle("Pre-diagnosis (> 90 days) elevated FCAL observations per subject") +
  xlab("Number of observations per subject") +
  ylab("Frequency")

table(mydf$n.before.diag)
table(mydf$n.high.before.diag)

# examine some examples
aux <- mydf$ids[mydf$n.high.before.diag == 5]
fcal.sorted %>% subset(ids %in% aux[1])
```

--->
