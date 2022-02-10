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
  stat = cumsumStat(s,e,X)
  b_0 = argmax(stat)
  cpt <- c()
  if (stat[b_0] > thresh){
    cpt <- append(cpt,stat[b_0])
    binseg(s,b_0,thresh)
    binseg(b_0+1,e,thresh)
  }
  return(cpt)
}


sig_1 <- abs(rnorm(100))
sig_2 <- abs(rnorm(100))
x1 <- rnorm(100,mean = (1:100)/20, sd = sig_1)
x2 <- rnorm(100,mean = (100:1)/20,sd = sig_2)
x <- c(x1**2,x2**2)
sig <- c(sig_1,sig_2)
plot(x)

make_parts<- function(n,m){
  part <- floor(n/m)
  for (i in 1:m){
    print()
  }
}


simulation_for_mean <- function(n, parts){
  part <- floor(n/parts)
  sig_1 <- abs(rnorm(mid))
  sig_2 <- abs(rnorm(n - mid))
  x1 <-  
}


 negloglike<- function(data, std){
   mu <- mean(data)
   negloglike <- sum(((data - mu)/std)**2 + log(2*pi*(std**2)))
   return(negloglike)
 }