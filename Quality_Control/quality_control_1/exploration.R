source("data_analysis.R")
data <- load.dataset("Quality_Control/quality_control_1/quality_control_1.json")
plot(data,col='red')
library("EnvCpt")
trend_model <- envcpt(data['V1'][,1] ,  model="trend")
trend_model$summary


trend_model$trend
plot(trend_model,type='bic')
