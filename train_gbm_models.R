# setwd("D:/Kaggle/GEFCOM2014/")
args = commandArgs(TRUE)
if (length(args)<1) {args[1]="0"}

lag = as.numeric(args[1])

cat("Number of lags =", lag, "\n")

source("gef2014_functions.R")

# PCA ####
load("weather_pca4.RData")

# GBMs ####
quantiles = c(.005, .01, .05, 0.1, 0.175, .25, .325, .4)
quantiles = c(quantiles, 0.5, 1-quantiles)
quantiles = sort(quantiles)

gbms = train_load_models_gbm(quantiles = quantiles, weather_pca = weather_pca,
                             max_trees = 750, shrinkages = c(0.075,0.3), lag_days = lag)

# assign(paste0("gbms", lag), gbms)

save(list = c(paste0("gbms"), "quantiles"), 
     file = paste0("lag", lag, "_load_models_gbm6.RData"))

send.text(paste("Lag", lag, "GBMs done training"))
