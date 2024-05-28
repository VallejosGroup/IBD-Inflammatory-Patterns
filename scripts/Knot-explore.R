library(ggplot2)
library(lme4)
require(optimx) # Required for the optimx optimizer used for LME

# load fcal data
libdr.data <- "/Volumes/igmm/cvallejo-predicct/libdr/"
fcal.cd <- read.csv(paste0(libdr.data, "processed/cd/fcal-diag.csv"))
fcal.uc <- read.csv(paste0(libdr.data, "processed/uc/fcal-diag.csv"))
fcal <- rbind(fcal.cd, fcal.uc)
rm(fcal.cd, fcal.uc) # Tidy up
fcal$calpro_result <- log(fcal$calpro_result)

# Define quantile percentages as per Harrell
quantiles <- list()
quantiles[[3]] <- c(0.1, 0.5, 0.9)
quantiles[[4]] <- c(0.05, 0.35, 0.65, 0.95)
quantiles[[5]] <- c(0.05, 0.275, 0.5, 0.725, 0.95)
quantiles[[6]] <- c(0.05, 0.23, 0.41, 0.59, 0.77, 0.95)
quantiles[[7]] <- c(0.025, 0.1833, 0.3417, 0.5, 0.6583, 0.8167, 0.975)

# Initialise vectors to hold AIC and BIC values
AIC.vals <- numeric()
BIC.vals <- numeric()

knots <- 3:7

for (i in knots) {
  message("Now processing ", i, " knots\n")
  model <- lmer(calpro_result ~
                 ns(calpro_time,
                    # place quantiles as above
                    knots = quantile(fcal$calpro_time,
                                     probs = quantiles[[i]])) +
                 # Random effect
                 (ns(calpro_time,
                     knots = quantile(fcal$calpro_time,
                                      probs = quantiles[[i]])) | ids),
               data = fcal,
               REML = FALSE, # Use maximum likelihood
               control = lmerControl(optimizer ='optimx',
                                     optCtrl=list(method='nlminb')))

  AIC.vals <- c(AIC.vals, round(AIC(model), 2))
  BIC.vals <- c(BIC.vals, round(BIC(model), 2))
  fit.data <- data.frame(calpro_time = seq(0, 10, by = 0.01),
                         # Require subject ID to make predictions
                         ids = unique(fcal$ids)[1])
  fit.data$y <- predict(model, newdata = fit.data)

  p <- ggplot(aes(x = calpro_time, y = calpro_result), data = fcal) +
    geom_point(alpha = 0.2) +
    geom_line(aes(x = calpro_time, y = y),
              data = fit.data,
              color = "#FB3640",
              linewidth = 1.2) +
    ggtitle(paste("K = ", i))

  ggsave(paste0("knots-",i, ".png"), p, width = 8, height = 4.5, units = "in")
}

DT::datatable(data.frame(Knots = 3:7, AIC = AIC.vals, BIC = BIC.vals),
              rownames = FALSE)
