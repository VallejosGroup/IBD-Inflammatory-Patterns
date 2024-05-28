library(ggplot2)
x <- seq(0, 10, by = 0.01)
y <- x + x * sin(x)
df <- data.frame(x = x, y = y)

centres <- c(2, 4, 6, 8)
l <- 1

grbfs <- data.frame(y = y,
                    grbf1 = exp((-(x - centres[1])^2) / (2 * (l^2))),
                    grbf2 = exp((-(x - centres[2])^2) / (2 * (l^2))),
                    grbf3 = exp((-(x - centres[3])^2) / (2 * (l^2))),
                    grbf4 = exp((-(x - centres[4])^2) / (2 * (l^2))))


grbf.mod <- lm(y ~ grbf1 + grbf2 + grbf3 + grbf4, data = grbfs)

predictions <- data.frame(time = x,
                          pred = grbf.mod$coefficients[2] * grbfs$grbf1,
                          grbf = 1)
predictions <- rbind(predictions,
                     data.frame(time = x,
                                pred = grbf.mod$coefficients[3] * grbfs$grbf2,
                                grbf = 2))
predictions <- rbind(predictions,
                     data.frame(time = x,
                                pred = grbf.mod$coefficients[4] * grbfs$grbf3,
                                grbf = 3))
predictions <- rbind(predictions,
                     data.frame(time = x,
                                pred =  grbf.mod$coefficients[5] * grbfs$grbf4,
                                grbf = 4))

predictions$grbf <- as.factor(predictions$grbf)


predicted <- data.frame(x = x,
                        y = grbf.mod$coefficients[1] +
                          grbf.mod$coefficients[2] * grbfs$grbf1 +
                          grbf.mod$coefficients[3] * grbfs$grbf2 +
                          grbf.mod$coefficients[4] * grbfs$grbf3 +
                          grbf.mod$coefficients[5] * grbfs$grbf4)


ggplot(data = df, aes(x = x, y = y)) +
  geom_line() +
  geom_line(data = predicted, aes(x = x, y = y), color = "#FF3562") +
  geom_line(data = predictions,
            aes(x = time, y = pred, color = grbf),
            linetype = 2) +
  scale_color_manual(values = c("#17BEBB", "#E7BBE3", "#0C6291", "#880D1E"),
                     name = "Scaled Gaussan\nradial basis\nfunctions",
                     labels = c("", "", "", "")) +
  geom_hline(yintercept = grbf.mod$coefficients[1],
             linetype = 3,
             color =  "#F18701") +
  theme_minimal() +
  annotate("text",
           label = "Predicted f(x)",
           x = 7.4,
           y = 16,
           angle = 74,
           color = "#FF3562") +
  annotate("text",
           label = "True f(x)",
           x = 9.55,
           y = 10,
           angle = -77,
           color = "black") +
  annotate("text",
           label = "Model intercept",
           x = 8,
           y = 2,
           color = "#F18701") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))




ggsave(filename = "paper/grbf-example.png",
       width = 8,
       height = 4.5,
       units = "in")

ggsave(filename = "paper/grbf-example.pdf",
       width = 8,
       height = 4.5,
       units = "in")


