# setwd("D:/Kaggle/GEFCOM2014/")
args = commandArgs(TRUE)
if (length(args)<1) {args[1]="a"}
if (length(args)<2) {args[2]="4"}
if (length(args)<3) {args[3]="100"}
if (length(args)<4) {args[4]="1e6"}

if (substring(args[1],1,1)=="e") {
  dist_method = "exact"
} else if (substring(args[1],1,1)=="a") {
  dist_method = "approximate"
}
parallel = as.numeric(args[2])
num_sims = as.numeric(args[3])
nrows = as.numeric(args[4])

cat("dist_method =", dist_method, ", cores =", parallel, "\n")

suppressMessages(library(lubridate))
suppressMessages(library(gbm))
suppressMessages(library(reshape2))
suppressMessages(library(gtools))
suppressMessages(library(mgcv))

cat("\nLoading data and functions\n")
source("gef2014_functions.R")
load("weather_pca4.RData")
load(paste0("lag", 0, "_load_models_gbm6.RData")) # for quantiles

set.seed(20141001)
pred_begin_datetime = ymd(20110301)+hours(1)
pred_end_datetime = ymd(20110401)

cat("\nSimulating weather data\n")
weather_sims = simulate_weather_pca(pred_begin_datetime = pred_begin_datetime, 
                                    pred_end_datetime = pred_end_datetime,
                                    weather_pca = weather_pca, 
                                    num_sims = num_sims)

weather_sims <- add_holidays(weather_sims)

sim_columns = grep("Sim",colnames(weather_sims))
weather_sims_m = melt(weather_sims, measure.vars = sim_columns,
                      value.name = "wPCA1")

cat("\nPredicting quantiles\n")
pred_sim_quantiles = matrix(NA, nrow(weather_sims_m), length(quantiles),
                            dimnames = list(NULL, quantiles))

for (d in 0:7) {
  cat("lag", d, "\n")
  if (d > 0) {
    # already loaded lag 0 gbms
    load(paste0("lag", d, "_load_models_gbm6.RData"))
  }
  
  for (h in 0:23) {
    if (d == 0 | d > 7) {
      this_hour = (weather_sims_m$Hour == h & weather_sims_m$DaysBack > 7)
    } else {
      this_hour = (weather_sims_m$Hour == h & weather_sims_m$DaysBack == d)
    }
    
    weather_sims_hour = weather_sims_m[this_hour, ]
    for (q in quantiles) {
      gbm_model = gbms[[paste(q, h)]]
      
      best.iter <- suppressWarnings(gbm.perf(gbm_model,method="cv", plot.it = FALSE))
      pred_sim_quantiles[this_hour, q==quantiles] = 
        predict(gbm_model, weather_sims_hour, n.trees = best.iter)
    }
  }
  rm(gbms)
  gc()
}


# delete this!!!
if (nrows < nrow(weather_sims)) {
  weather_sims = weather_sims[1:nrows, ]
}
# delete this!!!

cat("\nPredicting distribution\n")
start_time <- proc.time()
if (parallel > 1) {
  suppressMessages(library(doMC))
  registerDoMC(parallel)
  
  # TODO: remove everything from memory I won't need
  
  pred_distribution <- foreach(i=1:nrow(weather_sims), .combine=rbind) %dopar% {
    cat("Predict hour", i, "of", nrow(weather_sims),"\n")
    sim_matrix = pred_sim_quantiles[weather_sims_m$DateTime == weather_sims$DateTime[i],]
    cdf_functions = create_cdf_functions_list(sim_matrix, quantiles)
    c(i, distribution_forecast(cdf_functions, method = dist_method))
  }
  dimnames(pred_distribution) <- list(NULL, c("Row", 1:99/100))
} else {
  pred_distribution = matrix(NA, nrow(weather_sims), 99, dimnames = list(NULL, 1:99/100))
  for (i in 1:nrow(weather_sims)) {
    cat("Predict hour", i, "of", nrow(weather_sims),"\n")
    sim_matrix = pred_sim_quantiles[weather_sims_m$DateTime == weather_sims$DateTime[i],]
    cdf_functions = create_cdf_functions_list(sim_matrix, quantiles)
    pred_distribution[i, ] = distribution_forecast(cdf_functions, method = dist_method)
  }
}

cat("Time:", as.numeric(proc.time() - start_time)[3]/60, "minutes")

pred_distribution = data.frame(DateTime = weather_sims$DateTime, pred_distribution)
write.csv(pred_distribution, paste0("pred6_", paste(args,collapse = "_"), ".csv"), 
          row.names=FALSE)
