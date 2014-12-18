## Executive summary ##

Below is the summary of my solution. There are broadly two components: (1) simulate the weather and (2) predicting the distribution of the load, given the weather. 

To simplify the process, I did a principal component analysis of the 25 weather stations and only used the first principal component, since it contained over 80% of the variance. The first principal component (PC1) was close to being an average of the 25 stations. I found that the first principal component did better at predicting load than any single component. However, using all components together usually performed better than just the first principal component. 

The proceedure I used to simulate is detailed in task 4. Basically, I fit a normal model to estimate the normal weather on any given hour of the year. Then I analyzed the normalized residuals to look for autocorrelation. I used this autocorrelation and the normalized residuals of the most recent hours of actual weather to simulate the future.

To predict the distribution load, I built several gradient boosting machine models with quantile losses. The models included PC1 as well as several calendar variables. As the competitionl wore on, I also included lag variables for both PC1 and the load. These were only used for predicting the first few days of the month. I built separate models for each of the 24 hours. PC1 was the most important variable. As is detailed in task 4, for each hour, I built models for and predicted 21 different quantiles. I get predictions to all possible quantiles by fitting a smooth non-decreasing function to the 21 quantiles. 

For each of the simulated weather scenarios I get a predicted distribution. To combine the different simulations, I used the law of total probability, as outlined in task 4.

I have catalogued what I did for tasks 4 to 15, which correspond to weeks 1 to 12 of the competition. 

I have included the code used in the submission as well. Further, I have a version history of all the code used for each week, which can be provided if desired. The code includes the following files:

* `gef2014_functions.R`: Various functions used to load data, derive variables, and build models
* `train_pca_model.R`: Extracting principal components from the weather variables
* `train_gbm_models.R`: Training the quantile regression gradient boosting machine models
* `predict_distribution.R`: Simulating weather and predicting the distribution on the next month

## References ##

This competition made heavy use of the R statistical programming language. The packages crucial to the analysis include gbm and mgcv.

## Research Log ##

### Task 4 ###

#### Weather ####
I combine all 25 stations into one using the first principal component (using the covariance). To predict the weather for the upcoming month, I first estimate the normal mean and the normal variance of the first principal component using a generalized additive model with cyclic splines for day of year and hour of day as the covariates. I then get the normalized residuals (or "z-scores") by subtracting the normal mean and dividing by the normal standard deviation. I looked at the time series of these residuals. I first difference the series at 1 lag, then assume an AR1 process on the diffs. 

I simulate weather for the next month in a somewhat bootstrap manner, instead of estimating the AR1 parameter. I find what the most recent normalized diff is and then find the most similar normalized residual diffs in the past. I sample the month that follows from these most similar diffs, constraining on them starting at the same hour as the most previous hour. I simulated 500 weather series this way.

#### Load ####
For each hour, I built 21 quantile regression models using gradient boosting machines (GBMs) (so there were 24*21=504 models). The 21 percentiles used were:

`0.001 0.005 0.010 0.030 0.050 0.100 0.175 0.250 0.325 0.400 0.500 0.600 0.675 0.750 0.825, 0.900, 0.950 0.970 0.990 0.995 0.999`.

The covariates in each model are:

* The first principal component of the temperature from all the weather stations
* A nominal variable of the month
* A nominal variable of the day of week
* A continuous variable for day of year (normalized between 0 and 1)
* A continuous variable for trend (days since 1/1/2001)
* An indicator of whether the day is a holiday or not

For a given weather scenario and hour, I predict all 21 quantiles. I then use the 21 quantiles of load as a covariate and the 21 percentiles as a response and I build a cumulative distribution function (CDF) to predict percentile given any value of load. I do this with a monotonic spline with up to 5 degrees of freedom. For some simulated weather scenarios the CDF wouldn't make sense (because the predicted quantiles were not non-decreasing), so I threw these simulations out.

I combine the predicted CDFs from different simulated weather scenarios using the law of total probability.

\[ p_y = P(Y \leq y) = \sum_x P(Y \leq y|x) P(x) \]

This outputs $p_y$ when given $y$. What we want is the other way around. We can use bisection search find the appropriate load for a given percentile. This could be slow, so we only did this for the 0.01 and 0.99 percentiles and then evaluated the percentiles on a grid of loads between them and interpolated to get the final predictions.

**Note**: I made a mistake when implementing. I had two different versions of the temperature PCA saved. For one of them, I multiplied the loadings by -1 to make the loadings all positive and more interpretable. (I had also added in an extra month of data, but that had a small affect.)  I trained the GBMs with one version and predicted with the other. The test error of my submission was 14.365 (12th out of 28), but would have been 10.997 (4th out of 28) if I had used the correct PCA. 

I had also been training the GBMs with the additional covariates listed in task 5, but I failed to anticipate how long it would take. If it would have finished training on time, this model would have given an error of 10.122, good for first place out of 28.

### Task 5 ###

#### Weather ####
Same as for task 4.

#### Load ####
I used basically the same strategy as for task 4, except what follows.

I added in additional covariates for the first 7 days of the month. For the first 24 hours of the month, I also included the covariates below and trained separate GBMs for each hour.

* Load on same hour of previous day
* The first principal component of the temperature on same hour of previous day
* Load on the last hour of the previous day
* The first principal component of the temperature on the last hour of the previous day

For days 2 through 7, included the covariates above, but instead of previous day, I used the temperature and load from the most recent day, which is the last day of the previous month. To train these GBMs, I basically had 6 copies of the data, with 6 different lagged variables, as well as a categorical variable to tell how many days back the lagged variables were.

I also decreased the number of percentiles to increase speed. The percentiles used were 

`0.005 0.010 0.050 0.100 0.175 0.250 0.325 0.400 0.500 0.600 0.675 0.750 0.825 0.900 0.950 0.990 0.995`.

In summary, there were 3 GBM models for each hour and each quantile: 

1. A model with no lagged load or temperature for predicting days 8 to 28 of the month
2. A model with 1 day lagged load and temperature for predicting the first day of the month
3. A model with 2 to 7 days of lagged load and temperature, as well as a variable to tell how many days lag there is

The prediction resulted in an error of 10.10896, which was good for 4th place out of 28 submissions for the week.

### Task 6 ###

#### Experiments ####
I tried different predictors in the GBM for predicting median load to see improvement. I built the models 10 times for each hour and looked at the improvement. The results: 

* I tried projection pursuit regression to get a linear combination of the weather stations. This is hard to train and test because you have to use the load to get the projection, and then use load again for the modeling. I'm not sure there were significant improvements with it and there is high risk of over fitting.
* Adding the 2nd PC improved accuracy, but adding the 3rd, 4th, and 5th did not.
* I used the 25 weather stations instead of 2 PCs. The weather stations improved accuracy significantly.
* Adding lagged load improved predictions for the first 2 days. Adding lagged weather improved predictions a little bit for the first 2 days, primarily in the middle of the day (hours 8 to 18).

#### Weather ####
Same as for task 4.

#### Load ####
Same as for task 5 except for the following.

I build 8 different models (instead of 3) for each hour and quantile combination. The first used no lagged load or weather information. The second through eighth used lagged information from 1 to 7 days. These models were used to predict the first 7 days of the month. The first model was used to predict the rest of the month.

### Task 7 ###

#### Experiments ####

* Adding lagged load improved predictions for the first 2 days. Adding lagged weather improved predictions a little bit for the first 2 days, primarily in the middle of the day (hours 8 to 18).
* Adding wPC2 lag as well as wPC1 lag. Compared for 1 and 2 day lags. It might help a very little bit for lag 1 in the morning to afternoon.

#### Weather ####
Same as for task 4 except for the following.

I had to simulate the second weather pricipal component (wPCA2). I basically did this the same way as wPCA1. I further assumed that the 2 principal components were independent and simulated them independently.

I modelled the normal weather as a smooth function of day of year (same as before), but hour was used as a factor, interacted with month factor. So there are 12*24 free variables estimating hourly deviation from the daily temp. I used the same model for variation. I did the same for both principal components.

#### Load ####
Same as for task 6 except for the following.

I only built 3 models for each hour and quantile combination. One with no lagged load or weather, one with lags from the previous day, and one for lags from 2 days prior. I found in validations that load/weather from 3+ days prior did not improve accuracy of predicting the median.

I added the second pricipal component of weather (but not any lags).

I removed IsHoliday and replaced it with IsMemorial since predicting May. After the fact, I noticed that it did not make it into any of the trees that I looked at, so it was very insignificant.

My rank this week was 22 out of 34.

### Task 8 ###

#### Weather ####
Same as for task 7 except for the following. I removed wPCA2 from the model, so I don't need to simulate it.

#### Load ####
Same as for task 7 except for the following. I removed the covariates wPCA2 and IsMemorial.

### Task 9-15 ###

#### Weather ####
Same as for task 8.

#### Load ####
Same as for task 8.
