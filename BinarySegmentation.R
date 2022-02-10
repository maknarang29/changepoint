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

x1 <- rnorm(100,mean = (1:100)/20, sd = abs(rnorm(100)))
x2 <- rnorm(100,mean = (100:1)/20,sd = abs(rnorm(100)))
x <- c(x1,x2)

cum <- cumsumStat(2,198,x)
 thresh<- sqrt(2*log(length(x)))