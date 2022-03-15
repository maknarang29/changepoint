library(changepoint)
library(EnvCpt)


simulation_var <- function(n){
  m = n/100
  
  tau <- runif(m,min =30,max = n-30)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  sig <- rlnorm(m,0,log(10)/2)
  tau <- floor(tau)
  
  j = 1
  while (j<m) {
    x[tau[j]:tau[j+1]] = rnorm(tau[j+1] - tau[j]+1,0,sig[j])
    j = j+1
  }
  x[1:tau[1]] = rnorm(tau[1],0,sig[1])
  x[tau[m]:n] = rnorm(n - tau[m]+1,0,sig[length(sig)])
  #fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))
  return(list(x,tau,sig))
}



simulation_mean <- function(n){
  m = n/100
  
  tau <- runif(m,min =30,max = n-30)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  mu<- rnorm(m,0,5)
  tau <- floor(tau)
  
  j = 1
  while (j<m) {
    x[tau[j]:tau[j+1]] = rnorm(tau[j+1] - tau[j]+1,mu[j],1)
    j = j+1
}

  x[1:tau[1]] = rnorm(tau[1],mu[1],1)
  x[tau[m]:n] = rnorm(n - tau[m]+1,mu[length(mu)],1)
  #fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))
  return(list(x,tau,mu))
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
  mu<- rnorm(m,0,5)
  tau <- floor(tau)

  j = 1
  while (j<m) {
    x[tau[j]:tau[j+1]] = rnorm(tau[j+1] - tau[j]+1,mu[j+1],sig[j+1])
    j = j+1
  }

  x[1:tau[1]] = rnorm(tau[1],mu[1],sig[1])
  x[tau[m]:n] = rnorm(n - tau[m]+1,mu[length(mu)],sig[length(sig)])
  return(list(x,tau,mu,sig))
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
  alpha <- rnorm(m,0,runif(1,1,5))
  beta <- rnorm(m,0,runif(1,0,0.25))
  tau <- floor(tau)

  j = 1
  while (j<m) {
    index <- 1:(tau[j+1] - tau[j]+1)
    mean <- alpha[j+1] + beta[j+1]*index
    x[tau[j]:tau[j+1]] = rnorm(tau[j+1] - tau[j]+1,mean,sig[j+1])
    j = j+1
  }

  index <- 1:(tau[1])
  mean <- alpha[1] + beta[1]*index
  x[1:tau[1]] = rnorm(tau[1],mean,sig[1])
  index <- 1:(n - tau[m]+1)
  mean <- alpha[length(sig)] + beta[length(sig)]*index
  x[tau[m]:n] = rnorm(n - tau[m]+1,mean,sig[length(sig)])
  return(list(x,tau,alpha,beta,sig))
}


simulation_distribution <- function(n){
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
      shape_1 <- runif(1,0,10)
      shape_2 <- runif(1,0,10)
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
      location<- runif(1,0,10)
      scale <- runif(1,0,10)
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
      df<- runif(1,0,10)
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
  return(x)
}

