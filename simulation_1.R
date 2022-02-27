n=2000
m = n/100

tau <- runif(m,min =2,max = n-2)
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
  
  tau <- runif(m,min =2,max = n-2)
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
  
  tau <- runif(m,min =2,max = n-2)
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

  tau <- runif(m,min =2,max = n-2)
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



rm(n)
rm(m)
rm(x)
rm(tau)
rm(tau_num)
rm(sig)
rm(j)
rm(fit)
