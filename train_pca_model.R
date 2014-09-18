source("gef2014_functions.R")

# PCA ####
dat_train = open_data()
weather_pca = princomp(subset(dat_train, , w1:w25))
weather_pca = list(loadings = weather_pca$loadings,
                   center = weather_pca$center,
                   scale = weather_pca$scale)
class(weather_pca) <- "princomp"
if (all(weather_pca$loadings[,1] < 0)) {
  weather_pca$loadings[,1] = weather_pca$loadings[,1] * -1
}
save(weather_pca, file = "weather_pca4.RData")
