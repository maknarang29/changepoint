source("data_analysis.R")
data <- load.dataset("Quality_Control/quality_control_3/quality_control_3.json")
plot(data)
library("EnvCpt")
trend_model <- envcpt(data['V1'][,1] ,  model="trend")
trend_model$summary


trend_model$trend
