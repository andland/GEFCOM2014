GEFCOM2014
==========

This repository has the code and description of the modelling approach that I used for the Load track of the [Global Energy Forecasting Competition: 2014](http://www.drhongtao.com/gefcom), hosted on [CrowdAnalytix](https://www.crowdanalytix.com/).

### Description of files ###

* [`final_report.md`](final_report.md): A summary of my modelling approach as well as a log of what changed week by week
* [`gef2014_functions.R`](gef2014_functions.R): Various functions used to load data, derive variables, and build models
* [`train_pca_model.R`](train_pca_model.R): Extracting principal components from the weather variables
* [`train_gbm_models.R`](train_gbm_models.R): Training the quantile regression gradient boosting machine models
* [`predict_distribution.R`](predict_distribution.R): Simulating weather and predicting the distribution on the next month
