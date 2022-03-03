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
x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,mu[length(mu)],sig[length(sig))
plot(x)
library(changepoint)
library(EnvCpt)
fit<- cpt.var(data = x, method = "PELT")
fit
ncpts(fit)
m

plot(fit)



simulation_var <- function(n){
  m = n/100
  
  tau <- runif(m,min =30,max = n-30)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  sig <- rlnorm(m,0,log(10)/2)
  tau_num <- floor(tau)
  
  j = 1
  while (j<m) {
    x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,0,sig[j])
    j = j+1
  }
  x[1:tau_num[1]] = rnorm(tau_num[1],0,sig[1])
  x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,0,sig[length(sig)])
  #fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))
  return(list(x,tau_num,sig))
}



simulation_mean <- function(n){
  m = n/100
  
  tau <- runif(m,min =30,max = n-30)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  mu<- rlnorm(m,0,log(10)/2)
  tau_num <- floor(tau)
  
  j = 1
  while (j<m) {
    x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,mu[j],1)
    j = j+1
}

  x[1:tau_num[1]] = rnorm(tau_num[1],mu[1],1)
  x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,mu[length(mu)],1)
  #fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))
  return(list(x,tau_num,mu))
}


simulation_meanvar <- function(n){
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
  return(list(x,tau_num,mu,sig))
}

simulation_trend <- function(n){
  m = n/100

  tau <- runif(m,min = 30,max = n-30)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }

  x <- numeric(n)
  sig <- rlnorm(m,0,log(10)/2) 
  alpha <- rnorm(m,0,runif(1,20,50))
  beta <- rnorm(m,0,runif(1,0.1,1.5))
  tau_num <- floor(tau)

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
  return(list(x,tau_num,alpha,beta,sig))
}

data <- simulation_trend(3500)
plot(data[[1]]) 


n=2000
m = n/100

tau <- runif(m,min = 30,max = n-30)
tau <- sort(tau)
while (min(diff(tau))<30 ) {
  tau <- sort(runif(n = m,1,n))
}

x <- numeric(n)
sig <- rlnorm(m,0,log(10)/2) 
alpha <- rnorm(m,0,20)
beta <- rnorm(m,0,0.25)
tau_num <- floor(tau)

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

rm(n)
rm(m)
rm(x)
rm(tau)
rm(tau_num)
rm(sig)
rm(j)
rm(fit)
