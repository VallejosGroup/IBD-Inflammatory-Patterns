library(ggplot2)
times <- read.csv("Temp/fcal-times.csv")
ggplot(times, aes(x = ng, y = time)) +
  geom_bar(stat= "identity", color = "black", fill = "pink") +
  xlab("Number of assumed groups") +
  ylab("Time (minutes)") +
  ggtitle("Time taken to fit FCAL LCMMs ⏰",
          "25 cores; 50 grid search repetitions") +
  theme_minimal()

