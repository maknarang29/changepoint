source("simulation_1.R")
data <- simulation_mean(2000)
plot(data[[1]])
library(changepoint)
library(EnvCpt)
model <- cpt.mean(data[[1]],method= "PELT")
model
plot(model)
abline(v=data[[2]],col='green')
abline(v = cpts(model),col='blue')
data[[2]]
cpts(model)
tp<- 0

for (i in cpts(model)){
    cptlen <- c()
    for (j in seq(-5,5,1)){
        cptlen <- c(cptlen,i+j)
    }
    for (k in cptlen){
        if (k %in% data[[2]]){ tp <- tp + 1}
    }
}
tp
fp <- ncpts(model) - tp

?cpt.mean
truepos <- function(modelcpts,tau){
    tp<- 0
    for (i in modelcpts){
        cptrange <- c()
        for (j in seq(-5,5,1)){
            cptrange <- c(cptrange,i+j)
        }
        for (k in cptrange){
            if (k %in% tau){
                tp <- tp + 1
            }
        }
    }
    return(tp)
}
truepos(cpts(model),data[[2]])
