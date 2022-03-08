source("data_analysis.R")
data <- load.dataset("DATASET_2/gdp_argentina/gdp_argentina.json")
plot(data)
library("EnvCpt")
trend_model <- envcpt(data['V1'][,1] ,  model="trend")
trend_model$summary


trend_model$trend

