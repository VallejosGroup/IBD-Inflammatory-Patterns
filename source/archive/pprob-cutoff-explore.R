```{R}
myDF <- myDF %>% pivot_longer(cols = starts_with("prob"), names_to = "prob")

myDF %>% subset(class == 7)

myDF %>%
  subset(prob == "probmax")  %>%
  ggplot(aes(x = prob, y = value, fill = followup_cut)) +
  geom_violin(drop = FALSE) +
  xlab("Cluster") +
  ylab("Cluster assignment probability") +
  facet_wrap(.~n.total_cut, ncol = 4) +
  theme_classic()
```

```{R}
myDF %>%
  subset(class == 7) %>%
  ggplot(aes(x = prob, y = value, fill = followup_cut)) +
  geom_violin(drop = FALSE) +
  xlab("Cluster") +
  ylab("Cluster assignment probability") +
  ggtitle("FC 7") +
  theme_classic()
```

```{R}
myDF %>%
  subset(class == 1) %>%
  ggplot(aes(x = prob, y = value, fill = followup_cut)) +
  geom_violin(drop = FALSE) +
  xlab("Cluster") +
  ylab("Cluster assignment probability") +
  ggtitle("FC 1") +
  theme_classic()
```

```{R}
myDF %>%
  subset(class == 2) %>%
  ggplot(aes(x = prob, y = value, fill = followup_cut)) +
  geom_violin(drop = FALSE) +
  xlab("Cluster") +
  ylab("Cluster assignment probability") +
  ggtitle("FC 2") +
  theme_classic()
```

```{R}
myDF %>%
  ggplot(aes(x = prob, y = value, fill = followup_cut)) +
  geom_violin(drop = FALSE) +
  xlab("Cluster") +
  ylab("Cluster assignment probability") +
  facet_wrap(.~class, ncol = 5, nrow = 2) +
  theme_classic()
```
