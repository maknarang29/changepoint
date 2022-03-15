source("data_analysis.R")
data <- load.dataset("DATASET_2/gdp_croatia/gdp_croatia.json")
plot(data,col='blue')
library("EnvCpt")
library("changepoint")

colnames(data)
trend_model <- envcpt(data["GDP (constant LCU)"][,1])
trend_model$trend
plot(data)
plot(trend_model)
mod <- cpt.meanvar(data['V1'][,1])
(trend_model$trendar2cpt)
plot(mod)

?cpts.ts
data

?envcpt
