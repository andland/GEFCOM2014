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

cat("dist_method =", dist_method, ", cores =", parallel, ", sims =", num_sims, "\n")

suppressMessages(library(lubridate))
suppressMessages(library(gbm))
suppressMessages(library(reshape2))
suppressMessages(library(gtools))
suppressMessages(library(mgcv))

cat("\nLoading data and functions\n")
source("gef2014_functions.R")
load("weather_pca4.RData")
load(paste0("lag", 0, "_load_models_gbm9.RData")) # for quantiles

set.seed(20141029)
pred_begin_datetime = ymd(20110701)+hours(1)
pred_end_datetime = ymd(20110801)

cat("\nSimulating weather data\n")
weather_sims1 = simulate_weather_pca(pred_begin_datetime = pred_begin_datetime, 
                                     pred_end_datetime = pred_end_datetime,
                                     weather_pca = weather_pca, 
                                     num_sims = num_sims, which_pc = 1)
# weather_sims2 = simulate_weather_pca(pred_begin_datetime = pred_begin_datetime, 
#                                      pred_end_datetime = pred_end_datetime,
#                                      weather_pca = weather_pca, 
#                                      num_sims = num_sims, which_pc = 2)
weather_sims1 <- add_holidays(weather_sims1)

sim_columns = grep("Sim", colnames(weather_sims1))
weather_sims_m1 = melt(weather_sims1, measure.vars = sim_columns,
                      value.name = "wPCA1")
# sim_columns = grep("Sim", colnames(weather_sims2))
# weather_sims_m2 = melt(weather_sims2, measure.vars = sim_columns,
#                        value.name = "wPCA2")

# weather_sims_m = merge(weather_sims_m1, subset(weather_sims_m2, , c(DateTime, variable, wPCA2)))
weather_sims_m = weather_sims_m1
# rm(weather_sims_m1, weather_sims_m2, weather_sims2)

cat("\nPredicting quantiles\n")
pred_sim_quantiles = matrix(NA, nrow(weather_sims_m), length(quantiles),
                            dimnames = list(NULL, quantiles))

for (d in 0:2) {
  cat("lag", d, "\n")
  if (d > 0) {
    # already loaded lag 0 gbms
    load(paste0("lag", d, "_load_models_gbm9.RData"))
  }
  
  for (h in 0:23) {
    if (d == 0 | d > 2) {
      this_hour = (weather_sims_m$Hour == h & weather_sims_m$DaysBack > 2)
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
if (nrows < nrow(weather_sims1)) {
  weather_sims1 = weather_sims1[1:nrows, ]
}
# delete this!!!

cat("\nPredicting distribution\n")
start_time <- proc.time()
if (parallel > 1) {
  suppressMessages(library(doMC))
  registerDoMC(parallel)
  
  # TODO: remove everything from memory I won't need
  
  pred_distribution <- foreach(i=1:nrow(weather_sims1), .combine=rbind) %dopar% {
    cat("Predict hour", i, "of", nrow(weather_sims1),"\n")
    sim_matrix = pred_sim_quantiles[weather_sims_m$DateTime == weather_sims1$DateTime[i],]
    cdf_functions = create_cdf_functions_list(sim_matrix, quantiles)
    c(i, distribution_forecast(cdf_functions, method = dist_method))
  }
  dimnames(pred_distribution) <- list(NULL, c("Row", 1:99/100))
} else {
  pred_distribution = matrix(NA, nrow(weather_sims1), 99, dimnames = list(NULL, 1:99/100))
  for (i in 1:nrow(weather_sims1)) {
    cat("Predict hour", i, "of", nrow(weather_sims1),"\n")
    sim_matrix = pred_sim_quantiles[weather_sims_m$DateTime == weather_sims1$DateTime[i],]
    cdf_functions = create_cdf_functions_list(sim_matrix, quantiles)
    pred_distribution[i, ] = distribution_forecast(cdf_functions, method = dist_method)
  }
}

cat("Time:", as.numeric(proc.time() - start_time)[3]/60, "minutes")

pred_distribution = data.frame(DateTime = weather_sims1$DateTime, pred_distribution)
write.csv(pred_distribution, paste0("pred10_", paste(args,collapse = "_"), "_old.csv"), 
          row.names=FALSE)
