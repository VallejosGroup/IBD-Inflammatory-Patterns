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
