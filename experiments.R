source("simulation_1.R")
library(changepoint)
library(EnvCpt)


n=2000
m = n/100

tau <- runif(m,min =30,max = n-30)
tau <- sort(tau)
while (min(diff(tau))<30 ) {
  tau <- sort(runif(n = m,1,n))
}

x <- numeric(n)
sig <- rlnorm(m,0,log(10)/2)
mu<- rlnorm(m,0,log(10)/2)
tau_num <- floor(tau)

j = 1
while (j<m) {
  x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,mu[j+1],sig[j+1])
  j = j+1
}

x[1:tau_num[1]] = rnorm(tau_num[1],mu[1],sig[1])
x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,mu[length(mu)],sig[length(sig)])
plot(x)



fit<- cpt.var(data = x, method = "PELT")
fit
ncpts(fit)
m

plot(fit)



data <- simulation_trend(3500)
plot(data[[1]]) 


n=2000
m = n/100

tau <- runif(m,min = 30,max = n-30)
tau <- sort(tau)
while (min(diff(tau))<30 ) {
  tau <- sort(runif(n = m,1,n))
}
tau <- floor(tau)
dist_ind <- floor(runif(m,1,6))
x <- numeric(n)
for (i in 1:length(dist_ind)){
  if (dist_ind[i] == 1){
    shape_1 <- runif(1,2,10)
    shape_2 <- runif(1,2,10)
    if(i == 1){
      x[1:tau[1]] <- (-1)**sample(1:2,1)*runif(1,8,18)*rnorm(tau[1],shape_1,shape_2)
    }
    else if(i == m){
      x[tau[m]:n] <- (-1)**sample(1:2,1)*runif(1,8,18)*rnorm(n-tau[m]+1,shape_1,shape_2)
    }
    else{
      x[tau[i]:tau[i+1]] <- (-1)**sample(1:2,1)*runif(1,8,18)*rnorm(tau[i+1]-tau[i]+1, shape_1,shape_2)
    }
  }
  else if(dist_ind[i] == 2){
    location<- runif(1,2,10)
    scale <- runif(1,2,10)
    if(i == 1){
      x[1:tau[1]] <- rcauchy(tau[1],location,scale)
    }
    else if(i == m){
      x[tau[m]:n] <- rcauchy(n-tau[m]+1,location,scale)
    }
    else{
      x[tau[i]:tau[i+1]] <- rcauchy(tau[i+1]-tau[i]+1, location,scale)
    }
  }
  else if(dist_ind[i] == 3){
    df<- runif(1,10,40)
    if(i == 1){
      x[1:tau[1]] <- rchisq(tau[1],df)
    }
    else if(i == m){
      x[tau[m]:n] <- rchisq(n-tau[m]+1,df)
    }
    else{
      x[tau[i]:tau[i+1]] <- rchisq(tau[i+1]-tau[i]+1, df)
    }
  }
  else if(dist_ind[i] == 4){
    rate<- runif(1,0,10)
    if(i == 1){
      
      x[1:tau[1]] <- (-1)**sample(1:2,1)*runif(1,8,18)*rexp(tau[1],rate)
    }
    else if(i == m){
      x[tau[m]:n] <- (-1)**sample(1:2,1)*runif(1,8,18)*rexp(n-tau[m]+1,rate)
    }
    else{
      x[tau[i]:tau[i+1]] <- (-1)**sample(1:2,1)*runif(1,8,18)*rexp(tau[i+1]-tau[i]+1, rate)
    }
  }
  else if (dist_ind[i] == 5){
    lambda <- runif(1,0,10)
    if(i == 1){
      x[1:tau[1]] <- rpois(tau[1],lambda)
    }
    else if(i == m){
      x[tau[m]:n] <- rpois(n-tau[m]+1,lambda)
    }
    else{
      x[tau[i]:tau[i+1]] <- rpois(tau[i+1]-tau[i]+1,lambda)
    }
  }
}
x[which(x>100)] = rnorm(length(x[x>100]), 0 ,log(10)/2)
x[which(x< (-100))] = rnorm(length(x[x< (-100)]), 0 ,log(10)/2)

  plot(x)





rm(x)



vardat<- simulation_var(1000)
plot(vardat[[1]])

a <- cpt.var(vardat[[1]],method = "PELT")
plot(a)
abline(v = vardat[[2]])

vardat[[2]]




sig <- rlnorm(m,0,log(10)/2) 
alpha <- rnorm(m,0,20)
beta <- rnorm(m,0,0.25)

j = 1
while (j<m) {
  index <- 1:(tau_num[j+1] - tau_num[j]+1)
  mean <- alpha[j+1] + beta[j+1]*index
  x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,mean,sig[j+1])
  j = j+1
}

index <- 1:(tau_num[1])
mean <- alpha[1] + beta[1]*index
x[1:tau_num[1]] = rnorm(tau_num[1],mean,sig[1])
index <- 1:(n - tau_num[m]+1)
mean <- alpha[length(sig)] + beta[length(sig)]*index
x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,mean,sig[length(sig)])
plot(x)



f <- simulation_trend(2500)
mod <- cpts(f[[1]])
plot(f[[1]])
plot(mod)
abline(v = f[[2]])




?cpt.mean


rm(n)
rm(m)
rm(x)
rm(tau)
rm(tau_num)
rm(sig)
rm(j)
rm(fit)


meandata <- simulation_mean(2000)
plot(meandata[[1]])
meanmodel <- cpt.mean(meandata[[1]],method="PELT")
plot(meanmodel)

source("simulation_1.R")
trenddata <- simulation_trend(2000)
plot(trenddata[[1]])
trendmodel <- envcpt(trenddata[[1]],model="trendcpt")
plot(trendmodel)
trendmodel
trendmodel$trendcpt
cpts(trendmodel$trendcpt)



distdata <- simulation_distribution(200)
distdata
plot(distdata[[1]])
abline(v = distdata[[2]])


distdata[[3]]
