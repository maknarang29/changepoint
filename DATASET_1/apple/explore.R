source("data_analysis.R")
data <- load.dataset("DATASET_1/apple/apple.json")
colnames(data)
plot(y = data$Close,x=data$t,col='blue')
plot(data$Volume)
library("EnvCpt")
library("changepoint")

?cpts.ts
trend_model <- envcpt(data['V1'][,1],model= "trendcpt" )
trend_model$trend
plot(trend_model)
mod <- cpt.meanvar(data['V1'][,1])
(trend_model$trendar2cpt)
plot(mod)

?cpts.ts
data

?envcpt
