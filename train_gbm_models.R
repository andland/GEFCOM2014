source("gef2014_functions.R")

# PCA ####
load("weather_pca4.RData")

# GBMs ####
quantiles = c(.005, .01, .05, 0.1, 0.175, .25, .325, .4)
quantiles = c(quantiles, 0.5, 1-quantiles)
quantiles = sort(quantiles)

gbms = train_load_models_gbm(quantiles = quantiles, weather_pca = weather_pca,
                             max_trees = 750, shrinkages = c(0.075,0.3))
save(gbms, quantiles, file = "load_models_gbm5.RData")

gbm_lags = train_lag_load_models_gbm(quantiles = quantiles, weather_pca = weather_pca,
                             max_trees = 750, shrinkages = c(0.075,0.3))
save(gbm_lags, quantiles, file = "load_models_gbm_lag5.RData")

send.text("GBMs done training")
