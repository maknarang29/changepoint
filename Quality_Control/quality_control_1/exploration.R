library("rjson")
data <- fromJSON(file = "quality_control_1.json")
values <- data$series[[1]]$raw
timeindex <- data$time$index
plot(values)
library("EnvCpt")
trend_model <- envcpt(values, model="trend")
trend_model$summary

