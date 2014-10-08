# setwd("D:/Kaggle/GEFCOM2014/")
# rm(list = ls())

# functions for opening and manipulating data ####

season <- function(dates) {
  library(lubridate)
  dt = mdy(paste(month(dates), day(dates), 2012, sep = "/"), quiet = TRUE)
  ifelse(dt <= mdy(paste(3, 14, 2012, sep = "/"), quiet = TRUE), "Winter",
         ifelse(dt <= mdy(paste(5, 10, 2012, sep = "/"), quiet = TRUE), "Spring", 
                ifelse(dt <= mdy(paste(9, 20, 2012, sep = "/"), quiet = TRUE), "Summer", 
                       ifelse(dt <= mdy(paste(11, 15, 2012, sep = "/"), quiet = TRUE), "Fall", "Winter"))))
}

add_calendar_fields <- function(dat) {
  dat$Date = ymd(substring(dat$DateTime, 1, 10))
  dat$Year = year(dat$Date)
  dat$Month = month(dat$Date)
  dat$MonthFactor = factor(dat$Month)
  dat$Season = factor(season(dat$Date))
  dat$Hour = hour(dat$DateTime)
  dat$HourFactor = factor(dat$Hour)
  dat$DOW = factor(weekdays(dat$Date))
  dat$Trend = as.numeric(dat$Date - ymd("20010101"), units = "days") + 1
  dat$DOY = as.numeric(dat$Date - ymd(paste0(year(dat$Date),"0101")), units = "days")
  dat$DOY = dat$DOY / ifelse((dat$Year %% 4) == 0, 365, 364)
  # dat$HOY = as.numeric(dat$DateTime - ymd(paste0(year(dat$Date),"0101")))/(3600)
  
  return(dat)
}

add_holidays <- function(dat) {
  # library(tis) # don't load because `ymd` conflicts with lubridate
  library(lubridate)
  
  major_holidays = c("NewYears", "Memorial", "Independence", "Labor", "Thanksgiving", "Christmas")
  
  holidays = tis::federalHolidays(2001:2011, board = T, businessOnly = F)
  holidays = data.frame(Holiday = names(holidays), 
                        Date = ymd(holidays),
                        stringsAsFactors = FALSE)
  
  dat <- merge(dat, holidays, all.x = TRUE)
  dat <- dat[order(dat$DateTime), ]
  dat$IsHoliday <- as.numeric(!is.na(dat$Holiday))
  dat$Holiday[is.na(dat$Holiday)] <- "None"
  dat$IsMajorHoliday = as.numeric(dat$Holiday %in% major_holidays)
  
  dat$IsMemorial = as.numeric(dat$Holiday == "Memorial")
  dat$IsID4 = as.numeric(dat$Holiday == "Independence")
  dat$IsLabor = as.numeric(dat$Holiday == "Labor")
  dat$IsThankgiving = as.numeric(dat$Holiday == "Thanksgiving")
  dat$IsXmas = as.numeric(dat$Holiday == "Christmas")
  
  dat$Holiday = factor(dat$Holiday)
  
  # detach("package:tis", unload=TRUE)
  return(dat)
}

add_lags <- function(dat, lag_days = 1, which_pcs = 1) {
  # creates a lag variable for weather (wPCA1) and LOAD
  # from last hour of yesterday and same hour of yesterday
  if (lag_days <= 0) {
    return(dat)
  }
  dat_lag_day = subset(dat, , c("DateTime", paste0("wPCA", which_pcs), "LOAD"))
  dat_lag_day$DateTime = dat_lag_day$DateTime + days(lag_days)
  colnames(dat_lag_day)[2:ncol(dat_lag_day)] = paste0(colnames(dat_lag_day)[2:ncol(dat_lag_day)], "_lag_day")
  
  dat_lag_latest = subset(dat, Hour == 0, c("Date", paste0("wPCA", which_pcs), "LOAD"))
  colnames(dat_lag_latest)[2:ncol(dat_lag_day)] = paste0(colnames(dat_lag_latest)[2:ncol(dat_lag_day)], "_lag_latest")
  dat_lag_latest$Date = dat_lag_latest$Date + days(lag_days - 1)
  
  dat2 = merge(dat, dat_lag_latest, all.x = TRUE)
  dat2 = merge(dat2, dat_lag_day, all.x = TRUE)
  for (pc in which_pcs) {
    which_latest = which(colnames(dat2)==paste0("wPCA", pc, "_lag_latest"))
    which_day = which(colnames(dat2)==paste0("wPCA", pc, "_lag_day"))
    dat2[, which_latest] = ifelse(dat2$Hour == 0, dat2[, which_day], dat2[, which_latest])
  }
  dat2$LOAD_lag_latest = ifelse(dat2$Hour == 0, dat2$LOAD_lag_day, dat2$LOAD_lag_latest)
  
  dat2 <- dat2[order(dat2$DateTime), ]
  
  return(dat2)
}

open_data <- function(begin_datetime, end_datetime) {
  library(lubridate)
  files = dir()
  files = files[grep("-train.csv", files)]
  dat = NULL
  for (f in 1:length(files)) {
    file_name = paste0("L", f, "-train.csv")
    if (!(file_name %in% files)) {
      stop("Training files not in consecutive order!\nFile ",file_name," is expected.")
    }
    
    this_dat = read.csv(file_name, stringsAsFactors = FALSE)
    
    if (is.null(dat)) {
      dat = this_dat
    } else {
      dat = rbind(dat, this_dat)
    }
  }
  dat$DateTime = ymd(20010101) + hours(1:nrow(dat))
  
  dat <- add_calendar_fields(dat)
  
  if (missing(begin_datetime)) {
    begin_datetime = min(dat$DateTime)
  }
  if (missing(end_datetime)) {
    end_datetime = max(dat$DateTime)
  }
  
  dat <- subset(dat, DateTime >= begin_datetime & DateTime <= end_datetime)
  
  # lagged weather variables
  
  return(dat)
}

# functions for training and predicting the weather ####

train_weather_normals <- function(begin_datetime, end_datetime) {
  library(mgcv)
  dat_train = open_data(begin_datetime, end_datetime)
  
  normal_mean_models = list()
  normal_var_models = list()
  weather_stations = paste0("w", 1:25)
  for (w in weather_stations) {
    y_train = dat_train[,w]
    form = as.formula(paste0(w, " ~ s(DOY, bs = 'cc') + s(Hour, bs = 'cc')"))
    
    weights = rep(1, nrow(dat_train))
    fitted_old = rep(mean(y_train), nrow(dat_train))
    for (i in 1:5) {
      gam_model = gam(form, data = dat_train, weights = weights)
      dat_train$Fitted = fitted.values(gam_model)
      dat_train$Residuals = y_train - dat_train$Fitted
      
      gam_var_model = gam(log(Residuals^2) ~ s(DOY, bs = "cc") + s(Hour, bs = "cc"), data = dat_train)
      weights_old = weights
      fitted_var = exp(fitted(gam_var_model))
      weights = fitted_var^-1
      
      # print(min(fitted_var))
      
      cat(w, i, mean(abs(fitted_old-dat_train$Fitted)), mean(abs(weights_old-weights)), "\n")
      fitted_old = dat_train$Fitted
    }
    
    normal_mean_models[[w]] = gam_model
    normal_var_models[[w]] = gam_var_model
  }
  
  return(list(normal_mean_models, normal_var_models))
}

# normal_models = train_weather_normals()
# save(normal_models, file = "normal_models3.RData")

train_pca_normals <- function(begin_datetime, end_datetime, weather_pca, which_pc = 1) {
  library(mgcv)
  dat_train = open_data(begin_datetime, end_datetime)
  dat_train$wPCA = predict(weather_pca, dat_train)[, which_pc]
  
  y_train = dat_train$wPCA
  
  weights = rep(1, nrow(dat_train))
  fitted_old = rep(mean(y_train), nrow(dat_train))
  for (i in 1:5) {
    gam_mean_model = gam(wPCA ~ s(DOY, bs = 'cc') + HourFactor * MonthFactor,
                         data = dat_train, weights = weights)
    dat_train$Fitted = fitted.values(gam_mean_model)
    dat_train$Residuals = y_train - dat_train$Fitted
    
    gam_var_model = gam(log(Residuals^2) ~ s(DOY, bs = 'cc') + HourFactor * MonthFactor,
                        data = dat_train)
    weights_old = weights
    fitted_var = exp(fitted(gam_var_model))
    weights = fitted_var^-1
    
    # print(min(fitted_var))
    
    cat(i, mean(abs(fitted_old-dat_train$Fitted)), mean(abs(weights_old-weights)), "\n")
    fitted_old = dat_train$Fitted
  }
  
  return(list(gam_mean_model, gam_var_model))
}

# dat_train = open_data()
# weather_pca = princomp(subset(dat_train, , w1:w25))
# 
# normal_pca_models = train_pca_normals(weather_pca = weather_pca)
# save(weather_pca, normal_pca_models, file = "weather_pca3.RData")


predict_normals <- function(begin_datetime, end_datetime, normal_models) {
  library(lubridate)
  
  num_hours = as.numeric(end_datetime - begin_datetime, units = "hours") + 1
  dat_mean = data.frame(DateTime = begin_datetime + hours(0:(num_hours-1)))
  dat_mean <- add_calendar_fields(dat_mean)
  dat_sd = dat_mean
  
  weather_stations = paste0("w", 1:25)
  for (w in weather_stations) {
    dat_mean$Mean = as.numeric(predict(normal_models[[1]][[w]], dat_mean))
    colnames(dat_mean)[colnames(dat_mean)=="Mean"] = w
    
    dat_sd$SD = as.numeric(sqrt(exp(predict(normal_models[[2]][[w]], dat_mean))))
    colnames(dat_sd)[colnames(dat_sd)=="SD"] = paste0(w, "_sd")
  }
  
  return(list(dat_mean = dat_mean,
              dat_sd = dat_sd))
}

# pred_normal = predict_normals(ymd_hms(20101201010000), ymd_hms("20110101 00:00:00"), normal_models)
# save(pred_normal, file = "normal_preds3.RData")

predict_normal_pca <- function(begin_datetime, end_datetime, normal_models, which_pc = 1) {
  library(lubridate)
  
  num_hours = as.numeric(end_datetime - begin_datetime, units = "hours") + 1
  dat = data.frame(DateTime = begin_datetime + hours(0:(num_hours-1)))
  dat <- add_calendar_fields(dat)
  
  dat$wPCA_mean = as.numeric(predict(normal_models[[1]], dat))
  dat$wPCA_sd = as.numeric(sqrt(exp(predict(normal_models[[2]], dat))))
  # colnames(dat)[colnames(dat)=="wPCA_mean"] <- paste0("wPCA", which_pc, "_mean")
  # colnames(dat)[colnames(dat)=="wPCA_sd"] <- paste0("wPCA", which_pc, "_sd")
  
  return(dat)
}

# pred_normal_pca = predict_normal_pca(ymd_hms(20101201010000), ymd_hms("20110101 00:00:00"), normal_pca_models)
# save(pred_normal, file = "normal_preds3.RData")


simulate_weather_pca <- function(begin_datetime, end_datetime, 
                                 pred_begin_datetime, pred_end_datetime, 
                                 weather_pca, num_sims, which_pc = 1) {
  # a temperature must exist for pred_begin_datetime - 1 hour
  # Assumes independence between the PCs
  # setwd("D:/Kaggle/GEFCOM2014/")
  # load("weather_pca4.RData")
  # begin_datetime = ymd(20090101)
  # end_datetime = ymd(20110301)
  # pred_begin_datetime = ymd(20110301)+hours(1)
  # pred_end_datetime = ymd(20110401)
  # num_sims = 10; which_pc = 2
  
  library(mgcv)
  dat_start = open_data(pred_begin_datetime - hours(24), pred_begin_datetime - hours(1))
  if (nrow(dat_start) < 24) {
    stop("The prediction begin date is too far into the future!")
  }
  dat_train = open_data(begin_datetime, end_datetime)
  
  dat_train$wPCA = predict(weather_pca, dat_train)[, which_pc]
  
  normal_pca_models = train_pca_normals(begin_datetime, end_datetime, weather_pca = weather_pca, which_pc = which_pc)
  fitted_normal_pca = predict_normal_pca(min(dat_train$DateTime), max(dat_train$DateTime), normal_pca_models, which_pc = which_pc)
  
  rows = nrow(dat_train)
  dat_train = merge(dat_train, subset(fitted_normal_pca, , c(DateTime, wPCA_mean, wPCA_sd)))
  if (nrow(dat_train)!=rows) {
    stop("Unsuccessful merge")
  }
  
  dat_train$Resid = with(dat_train, (wPCA - wPCA_mean) / wPCA_sd)
  dat_train$Resid_Diff = c(NA, diff(dat_train$Resid))
  
  dat_start$wPCA = predict(weather_pca, dat_start)[, which_pc]
  pred_normal_pca_start = predict_normal_pca(pred_begin_datetime - hours(24), pred_begin_datetime - hours(1), normal_pca_models, which_pc = which_pc)
  wPCA_resids = (dat_start$wPCA - pred_normal_pca_start$wPCA_mean) / pred_normal_pca_start$wPCA_sd
  wPCA_resid_start = wPCA_resids[length(wPCA_resids)]
  wPCA_diff_start = wPCA_resids[length(wPCA_resids)] - wPCA_resids[length(wPCA_resids) - 1]
  
  pred_normal_pca = predict_normal_pca(pred_begin_datetime, pred_end_datetime, normal_pca_models, which_pc = which_pc)
  num_hours = nrow(pred_normal_pca)
  
  # append actuals from the previous day to the prediction data set
  # in order to get lags
  if (which_pc == 1) {
    dat_start$wPCA1 = dat_start$wPCA
    pred_normal_pca$LOAD = NA
    pred_normal_pca$wPCA1 = NA
    dat_start$wPCA_mean = NA
    dat_start$wPCA_sd = NA
    pred_normal_pca = rbind(subset(dat_start, , colnames(pred_normal_pca)),
               pred_normal_pca)
    temp_pred_normal_pca = pred_normal_pca
    
    pred_normal_pca$DaysBack = NA
    pred_normal_pca$LOAD_lag_day = NA
    pred_normal_pca$LOAD_lag_latest = NA
    pred_normal_pca$wPCA1_lag_day = NA
    pred_normal_pca$wPCA1_lag_latest = NA
    for (d in 1:2) {
      temp_df = add_lags(temp_pred_normal_pca, d, which_pc)
      rows = which(!is.na(temp_df$wPCA1_lag_day))
      pred_normal_pca$wPCA1_lag_day[rows] = temp_df$wPCA1_lag_day[rows]
      pred_normal_pca$LOAD_lag_day[rows] = temp_df$LOAD_lag_day[rows]
      pred_normal_pca$wPCA1_lag_latest[rows] = temp_df$wPCA1_lag_latest[rows]
      pred_normal_pca$LOAD_lag_latest[rows] = temp_df$LOAD_lag_latest[rows]
      pred_normal_pca$DaysBack[rows] = d
    }
    pred_normal_pca$DaysBack[is.na(pred_normal_pca$DaysBack)] = 33
    # pred_normal_pca$DaysBackFactor = factor(pred_normal_pca$DaysBack, levels = 1:2)
    
    # return the data back to normal
    pred_normal_pca = subset(pred_normal_pca, DateTime >= pred_begin_datetime)
    pred_normal_pca$LOAD = NULL
    pred_normal_pca$wPCA1 = NULL
    if (num_hours != nrow(pred_normal_pca)) {
      stop("Adding lags messed up weather prediction data")
    }
  }
  
  # eligable_months = month(pred_begin_datetime) + -1:1
  # eligable_months = ifelse(eligable_months > 12, eligable_months - 12, eligable_months)
  # eligable_months = ifelse(eligable_months < 1, eligable_months + 12, eligable_months)
  # starting_points = which(dat_train$Hour == hour(pred_begin_datetime) & 
  #                           !is.na(dat_train$Resid_Diff) & 
  #                           dat_train$Month %in% eligable_months)
  
  # get the closest diffs from the same hour
  starting_points = which(dat_train$Hour == hour(pred_begin_datetime) & 
                          !is.na(dat_train$Resid_Diff))
  starting_points = starting_points[starting_points < (nrow(dat_train) - num_hours - 1)]
  starting_points = order(abs(dat_train$Resid_Diff[starting_points-1] - wPCA_diff_start))[1:num_sims] + 1
  
  
  pca_sims = matrix(NA, num_hours, num_sims, dimnames = list(NULL, paste0("Sim", 1:num_sims)))
  bs_starts = sample(starting_points, num_sims) #reorders for no reason
  for (i in 1:num_sims) {
    bs_start = bs_starts[i]
    sim_diffs = dat_train$Resid_Diff[bs_start:(bs_start + num_hours - 1)]
    sim_resids = wPCA_resid_start + cumsum(sim_diffs)
    pca_sims[,i] = sim_resids * pred_normal_pca$wPCA_sd + pred_normal_pca$wPCA_mean
  }
  
  return(cbind(pred_normal_pca, pca_sims))
}

# functions for training the load models ####

train_load_models_gbm <- function(begin_datetime, end_datetime, quantiles, weather_pca,
                                  max_trees = 500, shrinkages = c(0.1,0.3), lag_days = 0) {
  suppressMessages(library(gbm))
  
  cat("load data\n")
  dat_train = open_data(begin_datetime, end_datetime)
  dat_train$wPCA1 = predict(weather_pca, dat_train)[, 1]
  dat_train$wPCA2 = predict(weather_pca, dat_train)[, 2]
  dat_train <- add_lags(dat_train, lag_days)
  dat_train <- add_holidays(dat_train)
  
  dat_train = subset(dat_train, !is.na(LOAD))
  if (lag_days > 0) {
    dat_train = subset(dat_train, !is.na(LOAD_lag_day) & !is.na(LOAD_lag_latest))
  }
  
  standard_columns = c("LOAD", "wPCA1", "wPCA2", "Hour", "MonthFactor", "DOW", "DOY", "Trend", "IsMemorial")
  lag_day_columns = c("wPCA1_lag_day", "LOAD_lag_day")
  lag_latest_columns = c("wPCA1_lag_latest", "LOAD_lag_latest")
  
  cat("run models\n")
  gbms = list()
  # shrinkages = shrinkage * 2^(-2:2)
  ptm <- proc.time()
  time_elapsed = rep(NA, length(quantiles) * 24)
  for (q in quantiles) {
    for (h in 0:23) {
      cat(q, "", h, "")
      
      columns = if (lag_days > 0) {
        if (h==0) {
          c(standard_columns, lag_day_columns)
        } else {
          c(standard_columns, lag_day_columns, lag_latest_columns)
        }
      } else {
        standard_columns
      }
      dat_gbm = subset(dat_train, Hour == h, columns)
      
      min_cv_error = Inf
      cv_errors = rep(NA, length(shrinkages))
      min_shrink = 0
      for (sh in shrinkages) {
        gbm_mod = gbm(LOAD ~ . - Hour, data = dat_gbm,
                      distribution = list(name="quantile", alpha = q),
                      n.trees = max_trees, shrinkage = sh, interaction.depth = 2,
                      keep.data = FALSE, cv.folds = 4, verbose = F, n.cores = 4)
        cv_errors[sh==shrinkages] = min(gbm_mod$cv.error)
        
        if(cv_errors[sh==shrinkages] < min_cv_error) {
          min_cv_error = cv_errors[sh==shrinkages]
          gbm_model = gbm_mod
          min_shrink = sh
        }
      }
      
      best.iter <- suppressWarnings(gbm.perf(gbm_model, method="cv", plot.it = F))
      cat("Shrinkage:", min_shrink, "Iters:", best.iter,"")
      
      which_gbm = (which(q==quantiles)-1)*24+h
      time_elapsed[which_gbm]=as.numeric(proc.time()-ptm)[3]/3600
      gbms_remain = length(time_elapsed) - which_gbm
      if (which_gbm >= 5) {
        slope = lm(y~x, data.frame(y=time_elapsed[(which_gbm-4):which_gbm], x=(which_gbm-4):which_gbm))$coefficients[2]
        time_remain = gbms_remain * slope
        cat(round(time_elapsed[which_gbm],1),"hours elapsed. Min",round(time_remain,1),"hours remain.\n")
      } else {
        cat("\n")
      }
      
      gbms[[paste(q, h)]] = gbm_model
    }
  }
  
  return(gbms)
}

train_lag_load_models_gbm <- function(begin_datetime, end_datetime, quantiles, weather_pca,
                                      max_trees = 500, shrinkages = c(0.075,0.3), test_month) {
  suppressMessages(library(gbm))
  suppressMessages(library(sampling))
  
  cat("load data\n")
  dat_train = open_data(begin_datetime, end_datetime)
  dat_train$wPCA1 = predict(weather_pca, dat_train)[,1]
  dat_train = subset(dat_train, , -(w1:w25))
  for (d in 2:7) {
    dat_lag <- cbind(add_lags(dat_train, d), DaysBack = d)
    if (d == 2) {
      dat_lags = dat_lag
    } else {
      dat_lags = rbind(dat_lags, dat_lag)
    }
  }
  dat_train <- add_holidays(dat_lags)
  rm(dat_lags, dat_lag)
  gc()
  
  # not used
  if (missing(test_month)) {
    test_month = month(max(dat_train$DateTime))
    cat("Assuming test month is ", test_month,".\n", sep="")
  }
  eligable_months = test_month + -2:2
  eligable_months = ifelse(eligable_months > 12, eligable_months - 12, eligable_months)
  eligable_months = ifelse(eligable_months < 1, eligable_months + 12, eligable_months)
  
  dat_train = subset(dat_train, !is.na(LOAD) & !is.na(LOAD_lag_day) & !is.na(LOAD_lag_latest))
  # dat_train = subset(dat_train, Month %in% eligable_months)
  dat_train$DaysBackFactor = factor(dat_train$DaysBack)
  
  standard_columns = c("LOAD", "wPCA1", "Hour", "MonthFactor", "DOW", "DOY", "Trend", "IsHoliday", "DaysBackFactor")
  lag_day_columns = c("wPCA1_lag_day", "LOAD_lag_day")
  lag_latest_columns = c("wPCA1_lag_latest", "LOAD_lag_latest")
  
  cat("run models\n")
  gbm_lags = list()
  # shrinkages = shrinkage * 2^(-2:2)
  ptm <- proc.time()
  for (q in quantiles) {
    for (h in 0:23) {
      cat(q, "", h, "")
      
      columns =if (h==0) {
        c(standard_columns, lag_day_columns)
      } else {
        c(standard_columns, lag_day_columns, lag_latest_columns)
      }
      dat_gbm = subset(dat_train, Hour == h, columns)
      
      # Only select one of the lags for each day
      dat_gbm = dat_gbm[order(dat_gbm$Trend),]
      s = sampling::strata(dat_gbm, "Trend", size=rep(1,length(unique(dat_gbm$Trend))), method="srswor")
      dat_gbm = getdata(dat_gbm, s)
      dat_gbm = subset(dat_gbm, , -c(ID_unit, Prob, Stratum))
      
      min_cv_error = Inf
      cv_errors = rep(NA, length(shrinkages))
      min_shrink = 0
      for (sh in shrinkages) {
        gbm_mod = gbm(LOAD ~ . - Hour, data = dat_gbm,
                      distribution = list(name="quantile", alpha = q),
                      n.trees = max_trees, shrinkage = sh, interaction.depth = 2,
                      keep.data = FALSE, cv.folds = 4, verbose = F, n.cores = 4)
        cv_errors[sh==shrinkages] = min(gbm_mod$cv.error)
        
        if(cv_errors[sh==shrinkages] < min_cv_error) {
          min_cv_error = cv_errors[sh==shrinkages]
          gbm_model = gbm_mod
          min_shrink = sh
        }
      }
      
      best.iter <- suppressWarnings(gbm.perf(gbm_model, method="cv", plot.it = F))
      cat("Shrinkage:", min_shrink, "Iters:", best.iter,"")
      
      time.elapsed=as.numeric(proc.time()-ptm)[3]
      tot.time=length(quantiles)*24/((which(q==quantiles)-1)*24+h)*time.elapsed
      time.remain=tot.time-time.elapsed
      cat(round(time.elapsed/3600,1),"hours elapsed. Min",round(time.remain/3600,1),"hours remain.\n")
      
      gbm_lags[[paste(q, h)]] = gbm_model
    }
  }
  
  return(gbm_lags)
}

train_load_models_gam <- function(begin_datetime, end_datetime, quantiles, weather_pca) {
  library(mgcv)
  
  dat_train = open_data(begin_datetime, end_datetime)
  dat_train = subset(dat_train, !is.na(LOAD))
  dat_train <- add_holidays(dat_train)
  dat_train$wPCA1 = predict(weather_pca, dat_train)[,1]
  
  gam_model = gam(LOAD ~ s(wPCA1, by=HourFactor, k=5) + s(Hour, DOY) + MonthFactor + DOW + s(Trend, bs='cr', m=1),
                  data = dat_train)
  
  return(gam_model)
}

# functions for predicting the distribution ####

create_cdf_function <- function(load, quantiles) {
  library(mgcv)
  library(gtools)
  
  dist_dat = data.frame(x=load, y=quantiles)
  # plot(dist_dat)
  f.ug <- gam(logit(y)~s(x,k=5,bs="cr"), data = dist_dat)
  load_range = min(dist_dat$x):max(dist_dat$x)
  # lines(load_range,predict(f.ug, load_range))
  
  sm <- smoothCon(s(x,k=5,bs="cr"),dist_dat,knots=NULL)[[1]]
  F <- mono.con(sm$xp, TRUE);   # get constraints
  G <- list(X=sm$X, C=matrix(0,0,0), sp=f.ug$sp, p=sm$xp, y=logit(dist_dat$y), w=logit(dist_dat$y)*0+1)
  G$Ain <- F$A; G$bin <- F$b; G$S <- sm$S; G$off <- 0
  
  p <- pcls(G);  # fit spline (using s.p. from unconstrained fit)
  
  # fv<-Predict.matrix(sm,data.frame(x=load_range))%*%p
  # lines(load_range,inv.logit(fv),col=4)
  
  cdf_function <- function(load) {
    inv.logit(Predict.matrix(sm,data.frame(x=load))%*%p)
  }
  return(cdf_function)
}

# f = create_cdf_function(test[100,], quantiles)

combined_cdf <- function(cdf_functions, load) {
  library(mgcv)
  library(gtools)
  
  quantile = numeric(length(cdf_functions))
  for (i in 1:length(quantile)) {
    quantile[i] = cdf_functions[[i]](load)
  }
  return(mean(quantile))
}

quantile_forecast <- function(cdf_functions, quantile, load_min, load_max) {
  library(mgcv)
  library(gtools)
  
  if (missing(load_min)) {
    load_min = 0
  }
  if (missing(load_max)) {
    load_max = 500
  }
  
  cdf_min = combined_cdf(cdf_functions, load_min)
  while(cdf_min > quantile) {
    warning(paste("load_min =", load_min, "not small enough for quantile", quantile))
    load_min = load_min - 50
    cdf_min = combined_cdf(cdf_functions, load_min)
  }
  
  cdf_max = combined_cdf(cdf_functions, load_max)
  while(cdf_max < quantile) {
    warning(paste("load_max =", load_max, "not big enough for quantile", quantile))
    load_max = load_max + 50
    cdf_max = combined_cdf(cdf_functions, load_max)
  }
  
  counter = 0
  while((load_max-load_min) > 0.001) {
    load_try = (load_max + load_min) / 2
    cdf_try = combined_cdf(cdf_functions, load_try)
    
    if (cdf_try > quantile) {
      cdf_max = cdf_try
      load_max = load_try
    } else if (cdf_try < quantile) {
      cdf_min = cdf_try
      load_min = load_try
    } else {
      stop("cdf_try error")
    }
    
    counter = counter + 1
    if (counter > 50) {
      stop("Bisection search has taken 50 iterations")
    }
  }
  
  return((load_max + load_min) / 2)
}

distribution_forecast <- function(cdf_functions, method = "exact") {
  library(mgcv)
  library(gtools)
  
  load_min = 0
  load_max = 500
  dist_forecast = rep(NA, 99)
  
  if (method == "exact") {
    for (q in 1:49) {
      dist_forecast[q] = quantile_forecast(cdf_functions, q/100, load_min, load_max)
      load_min = dist_forecast[q]
      
      dist_forecast[100 - q] = quantile_forecast(cdf_functions, 1 - q/100, load_min, load_max)
      load_max = dist_forecast[100 - q]
    }
    dist_forecast[50] = quantile_forecast(cdf_functions, 0.5, load_min, load_max)
  } else if (method == "approximate") {
    dist_forecast[1] = quantile_forecast(cdf_functions, 1/100, load_min, load_max)
    dist_forecast[99] = quantile_forecast(cdf_functions, 99/100, dist_forecast[1], load_max)
    
    grid_length = 99
    load_grid = seq(dist_forecast[1], dist_forecast[99], length.out = grid_length)
    comb_cdf = rep(NA, grid_length)
    comb_cdf[c(1,grid_length)] = c(dist_forecast[1], dist_forecast[99])
    for (l in load_grid[-c(1,grid_length)]) {
      comb_cdf[l==load_grid] = combined_cdf(cdf_functions, l)
    }
    
    if (any(diff(load_grid) < 0)) {
      sf = splinefun(comb_cdf, load_grid)
    } else {
      sf = splinefun(comb_cdf, load_grid, method = "monoH.FC")
    }
    dist_forecast[2:98] = sf(2:98/100)
  } else {
    stop("method not specified correctly")
  }
  return(dist_forecast)
}

create_cdf_functions_list <- function(load_sims, quantiles) {
  cdf_functions = list()
  counter = 1
  for (i in 1:nrow(load_sims)) {
    f = create_cdf_function(load_sims[i, ], quantiles)
    
    # check to see if it is giving a reasonable cdf
    if (!is.na(f(0)) & !is.na(f(500))) {
      if (f(0) < 0.01 & f(500) > .99) {
        cdf_functions[[counter]] = f
        counter = counter + 1
      }
    }
  }
  cdf_functions
}

# Evaluation ####
quantile_loss <- function(pred, actual) {
  if ((nrow(pred) != length(actual)) | ncol(pred) != 99) {
    stop("dimensions are not correct")
  }
  
  loss = 0
  for (q in 1:99) {
    loss = loss + sum(ifelse(actual < pred[, q], (1-q/100)*(pred[, q]-actual),
                         (q/100)*(actual-pred[, q])))
  }
  loss / prod(dim(pred))
  
}

# in a separate file because it has my phone number
source("send_text_function.R")