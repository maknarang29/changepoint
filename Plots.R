source("simulation_1.R")
library(EnvCpt)
library(changepoint)
set.seed(1)

#####################################################################################

## Simulation for variance data

vardata<- simulation_var(2000)
## Plotting the simulation
jpeg("PLOTS/simulation_plots/variance_sim.jpg")
plot(vardata[[1]],xlab='x',ylab='y')
dev.off()

## Fitting PELT model
varmodel_PELT <- cpt.var(vardata[[1]],method = "PELT")
jpeg("PLOTS/PELT_plots/variance_PELT.jpg")
plot(varmodel_PELT,xlab='x',ylab='y')
dev.off()

## Adding True cpts PELT
jpeg("PLOTS/Truecpt_PELT/variance.jpg")
plot(varmodel_PELT,xlab='x',ylab='y')
abline(v = vardata[[2]],col = 'green')
dev.off()

## Fitting Binseg
varmodel_binseg <- cpt.var(vardata[[1]],method = "BinSeg",Q = 50)
jpeg("PLOTS/BinSeg_plots/variance_BinSeg.jpg")
plot(varmodel_binseg,xlab='x',ylab='y')
dev.off()

## Adding True cpts BinSeg
jpeg("PLOTS/Truecpt_BinSeg/variance.jpg")
plot(varmodel_binseg,xlab='x',ylab='y')
abline(v = vardata[[2]],col = 'green')
dev.off()

#####################################################################################
## Simulation for Mean

meandata<- simulation_mean(2000)

## Generating and plotting data
jpeg("PLOTS/simulation_plots/mean_sim.jpg")
plot(meandata[[1]],xlab='x',ylab='y')
dev.off()


## Fitting PELT
meanmodel_PELT <- cpt.mean(meandata[[1]],method = "PELT")
jpeg("PLOTS/PELT_plots/mean_PELT.jpg")
plot(meanmodel_PELT,xlab='x',ylab='y')
dev.off()
## Adding true cpts PELT
jpeg("PLOTS/Truecpt_PELT/mean.jpg")
plot(meanmodel_PELT,xlab='x',ylab='y')
abline(v = meandata[[2]],col = 'green')
dev.off()
## Fitting BinSeg
meanmodel_binseg <- cpt.mean(meandata[[1]],method = "BinSeg",Q = 50)
jpeg("PLOTS/BinSeg_plots/mean_BinSeg.jpg")
plot(meanmodel_binseg,xlab='x',ylab='y')
dev.off()
## Adding true cpts BinSeg
jpeg("PLOTS/Truecpt_BinSeg/mean.jpg")
plot(meanmodel_binseg,xlab='x',ylab='y')
abline(v = meandata[[2]],col = 'green')
dev.off()

#####################################################################################
## Simulating Meanvar Data
meanvardata<- simulation_meanvar(2000)

## Plotting the simulation
jpeg("PLOTS/simulation_plots/meanvariance_sim.jpg")
plot(meanvardata[[1]],xlab='x',ylab='y')
dev.off()

## Fitting PELT model
meanvarmodel_PELT <- cpt.meanvar(meanvardata[[1]],method = "PELT")
jpeg("PLOTS/PELT_plots/meanvariance_PELT.jpg")
plot(meanvarmodel_PELT,xlab='x',ylab='y')
dev.off()
## Adding true cpts PELT
jpeg("PLOTS/Truecpt_PELT/meanvariance.jpg")
plot(meanvarmodel_PELT,xlab='x',ylab='y')
abline(v = meanvardata[[2]],col = 'green')
dev.off()
## Fitting BinSeg Model
meanvarmodel_binseg <- cpt.meanvar(meanvardata[[1]],method = "BinSeg",Q =50)
jpeg("PLOTS/BinSeg_plots/meanvariance_BinSeg.jpg")
plot(meanvarmodel_binseg,xlab='x',ylab='y')
dev.off()
## Adding true cpts BinSeg
jpeg("PLOTS/Truecpt_BinSeg/meanvariance.jpg")
plot(meanvarmodel_binseg,xlab='x',ylab='y')
abline(v = meanvardata[[2]],col = 'green')
dev.off()

#####################################################################################

## Simulating Trend Data
trenddata <- simulation_trend(2000)

## Plotting the simulation
jpeg("PLOTS/simulation_plots/trend_sim.jpg")
plot(trenddata[[1]],xlab='x',ylab='y')
dev.off()

## Fitting the trendcpt model
trendmodel <- envcpt(trenddata[[1]],model="trendcpt")
jpeg("PLOTS/PELT_plots/trendcptmodel.jpg")
plot(trendmodel)
dev.off()

trendmodel
trendmodel$trendcpt
jpeg("PLOTS/Truecpt_PELT/trend.jpg")
plot(trenddata[[1]],xlab='x',ylab='y')
abline(v = trenddata[[2]],col="green")
abline(v = cpts(trendmodel$trendcpt),col="red")
dev.off()

#####################################################################################
 source("tradeoff.R")

## Variance Model PELT
vartpPELT<- truepos(cpts(varmodel_PELT),vardata[[2]])
## Variance Model Binseg
vartpBinSeg<- truepos(cpts(varmodel_binseg),vardata[[2]])
## Mean Model PELT
meantpPELT<- truepos(cpts(meanmodel_PELT),meandata[[2]])
## Mean model BinSeg
meantpBinSeg<- truepos(cpts(meanmodel_binseg),meandata[[2]])
## MeanVar model PELT
meanvartpPELT<- truepos(cpts(meanvarmodel_PELT),meanvardata[[2]]) 
## MeanVar model BinSeg
meanvartpBinSeg<- truepos(cpts(meanvarmodel_binseg),meanvardata[[2]])



############################################################################
## variance Simulation
source("simulation_1.R")
tplistPELTvariance<-c()
tplistBinSegvariance<-c()
fplistPELTvariance<-c()
fplistBinSegvariance<-c()

samplesizes <- floor(runif(100,2,25))
samplesizes <- samplesizes*100
for (i in seq(1,100)){
    tp<-0
    data <- simulation_var(samplesizes[i])
    model_PELT <- cpt.var(data[[1]],method="PELT")
    #model_binseg <- cpt.variance(data[[1]],method="BinSeg",Q = 50)
    tplistPELTvariance <- c(tplistPELTvariance,truepos(cpts(model_PELT),data[[2]])) 
    # fplistPELTvariance <- c(fplistPELTvariance,(ncpts(model_PELT)-truepos(cpts(model_PELT),data[[2]])))
    # acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistPELTvariance/(samplesizes/100))


for (i in seq(1,100)){
    data <- simulation_var(samplesizes[i])
    #model_PELT <- cpt.variance(data[[1]],method="PELT")
    model_BinSeg <- cpt.var(data[[1]],method="BinSeg",Q = 50)
    tplistBinSegmean <- c(tplistBinSegmean,truepos(cpts(model_BinSeg),data[[2]])) 
    #fplistBinSeg <- c(fplistBinSeg,(ncpts(model_BinSeg)-truepos(cpts(model_BinSeg),data[[2]])))
    #acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistBinSegmean/(samplesizes/100))



##################################################################################################
## Mean Simulation
tplistPELTmean<-c()
tplistBinSegmean<-c()
fplistPELTmean<-c()
fplistBinSegmean<-c()

samplesizes <- floor(runif(100,2,25))
samplesizes <- samplesizes*100
for (i in seq(1,100)){
    tp<-0
    data <- simulation_mean(samplesizes[i])
    model_PELT <- cpt.mean(data[[1]],method="PELT")
    #model_binseg <- cpt.mean(data[[1]],method="BinSeg",Q = 50)
    tplistPELTmean <- c(tplistPELTmean,truepos(cpts(model_PELT),data[[2]])) 
    # fplistPELTmean <- c(fplistPELTmean,(ncpts(model_PELT)-truepos(cpts(model_PELT),data[[2]])))
    # acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistPELTmean/(samplesizes/100))


for (i in seq(1,100)){
    data <- simulation_mean(samplesizes[i])
    #model_PELT <- cpt.mean(data[[1]],method="PELT")
    model_BinSeg <- cpt.mean(data[[1]],method="BinSeg",Q = 50)
    tplistBinSegmean <- c(tplistBinSegmean,truepos(cpts(model_BinSeg),data[[2]])) 
    #fplistBinSeg <- c(fplistBinSeg,(ncpts(model_BinSeg)-truepos(cpts(model_BinSeg),data[[2]])))
    #acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistBinSegmean/(samplesizes/100))

##################################################################################################
## Meanvar Simulation
tplistPELTmeanvar<-c()
tplistBinSegmeanvar<-c()
fplistPELTmeanvar<-c()
fplistBinSegmeanvar<-c()

samplesizes <- floor(runif(100,2,25))
samplesizes <- samplesizes*100
for (i in seq(1,100)){
    tp<-0
    data <- simulation_meanvar(samplesizes[i])
    model_PELT <- cpt.meanvar(data[[1]],method="PELT")
    #model_binseg <- cpt.mean(data[[1]],method="BinSeg",Q = 50)
    tplistPELTmeanvar <- c(tplistPELTmeanvar,truepos(cpts(model_PELT),data[[2]])) 
    # fplistPELTmeanvar <- c(fplistPELTmeanvar,(ncpts(model_PELT)-truepos(cpts(model_PELT),data[[2]])))
    # acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistPELTmeanvar/(samplesizes/100))


for (i in seq(1,100)){
    data <- simulation_meanvar(samplesizes[i])
    #model_PELT <- cpt.mean(data[[1]],method="PELT")
    model_BinSegmeanvar <- cpt.meanvar(data[[1]],method="BinSeg",Q = 50)
    tplistBinSegmeanvar <- c(tplistBinSegmeanvar,truepos(cpts(model_BinSeg),data[[2]])) 
    #fplistBinSeg <- c(fplistBinSeg,(ncpts(model_BinSeg)-truepos(cpts(model_BinSeg),data[[2]])))
    #acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistBinSegmeanvar/(samplesizes/100))

##################################################################################################
## trend Simulation
tplisttrend<-c()
tplistBinSegmeanvar<-c()
fplistPELTmeanvar<-c()
fplistBinSegmeanvar<-c()

samplesizes <- floor(runif(100,2,25))
samplesizes <- samplesizes*100
for (i in seq(1,100)){
    tp<-0
    data <- simulation_trend(samplesizes[i])
    model_trend <- envcpt(data[[1]],model="trendcpt")
    #model_binseg <- cpt.mean(data[[1]],method="BinSeg",Q = 50)
    tplisttrend <- c(tplisttrend,truepos(cpts(model_trend$trendcpt),data[[2]])) 
    # fplistPELTmeanvar <- c(fplistPELTmeanvar,(ncpts(model_PELT)-truepos(cpts(model_PELT),data[[2]])))
    # acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplisttrend[101:200]/(samplesizes/100))


for (i in seq(1,100)){
    data <- simulation_meanvar(samplesizes[i])
    #model_PELT <- cpt.mean(data[[1]],method="PELT")
    model_BinSegmeanvar <- cpt.meanvar(data[[1]],method="BinSeg",Q = 50)
    tplistBinSegmeanvar <- c(tplistBinSegmeanvar,truepos(cpts(model_BinSeg),data[[2]])) 
    #fplistBinSeg <- c(fplistBinSeg,(ncpts(model_BinSeg)-truepos(cpts(model_BinSeg),data[[2]])))
    #acc <- c(acc,(truepos(cpts(model_PELT),data[[2]])/samplesizes[i]/100))
}
mean(tplistBinSegmeanvar/(samplesizes/100))
