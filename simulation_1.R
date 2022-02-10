n=2000
m = n/100

tau <- runif(m,min =2,max = n-2)
tau <- sort(tau)
while (min(diff(tau))<30 ) {
  tau <- sort(runif(n = m,1,n))
}

x <- numeric(n)
sig <- numeric(m)
tau_num <- floor(tau)

j = 1
while (j<m) {
  sig[j] <- rlnorm(1,0,log(10)/2)
  x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,0,sig[j])
  j = j+1
}
x[1:tau_num[1]] = rnorm(tau_num[1],0,rlnorm(1,0,log(10)/2))
x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,0,rlnorm(1,0,log(10)/2))
plot(x)
library(changepoint)
library(EnvCpt)
fit<- cpt.var(data = x, method = "PELT")
fit
ncpts(fit)
m

plot(fit)



simulation <- function(n){
  m = n/100
  
  tau <- runif(m,min =2,max = n-2)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  sig <- numeric(m)
  tau_num <- floor(tau)
  
  j = 1
  while (j<m) {
    sig[j] <- rlnorm(1,0,log(10)/2)
    x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,0,sig[j])
    j = j+1
  }
  x[1:tau_num[1]] = rnorm(tau_num[1],0,rlnorm(1,0,log(10)/2))
  x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,0,rlnorm(1,0,log(10)/2))
  fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))
  return(x)
}





cumsumStat <- function(s,e,X){
  stopifnot(e>s)
  n = e-s+1
  ind <- 1
  b <- s
  Xt <- numeric(e-s)
  while (b<e) {
    Xt[ind] = sqrt((e-b)/(n*(b-s+1)))*sum(X[s:b]) - sqrt((b-s+1)/(n*(e-b)))*sum(X[(b+1):e])
    #print(sqrt((e-b)/(n*(b-s+1)))*sum(X[s:b]) - sqrt((b-s+1)/(n*(e-b)))*sum(X[(b+1):e]))
    #print(b)
    b = b+1
    ind = ind + 1
  }
  return(Xt)
}


binseg <- function(s,e,thresh,X){
  stopifnot(e-s>1)
  stat <- cumsumStat(s,e,X)
  b_0 <- which.max(stat)
  cpt <- c()
  if (stat[b_0] > thresh){
    cpt <- append(cpt,stat[b_0])
    binseg(s,b_0,thresh,X[s:b_0])
    binseg((b_0+1),e,thresh,X[(b_0+1):e])
  }
  return(cpt)
}

which.max(data)

data<- simulation(200)
cumstat<- cumsumStat(2,198,data)

thresh = sqrt(2*log(200))


models <- c("mean","meancpt","meanar1","meanar2","meanar1cpt","meanar2cpt","trend","trendcpt","trendar1","trendar2","trendar1cpt","trendar2cpt")



rm(n)
rm(m)
rm(x)
rm(tau)
rm(tau_num)
rm(sig)
rm(j)
rm(fit)
