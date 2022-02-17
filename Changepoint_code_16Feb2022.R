###Change in variance

simulation <- function(n){
  m = n/100
  
  tau <- runif(m,min =2,max = n-2)
  tau <- sort(tau)
  while (min(diff(tau))<30 ) {
    tau <- sort(runif(n = m,1,n))
  }
  
  x <- numeric(n)
  sig <- numeric(m+1)  ##There are m cpts, but m+1 segments
  tau_num <- floor(tau)
  
  
  sig[1] <- rlnorm(1,0,log(10)/2)  #You didn't save the first sigma
  x[1:tau_num[1]] = rnorm(tau_num[1],0,sig[1])

  j = 1
  while (j<m) {
    sig[j+1] <- rlnorm(1,0,log(10)/2)
    x[tau_num[j]:tau_num[j+1]] = rnorm(tau_num[j+1] - tau_num[j]+1,0,sig[j+1])
    j = j+1
  }
  sig[m+1] <- rlnorm(1,0,log(10)/2)   #You didn't save the last sigma!
  x[tau_num[m]:n] = rnorm(n - tau_num[m]+1,0,sig[m+1])


  fit<- cpt.var(data = x, method = "PELT")
  #return(fit,ncpts(fit))

  #Return all the information you need in a list
  return(list(x=x, sig=sig, tau = tau_num, fit=fit))
}

set.seed(30)
data <- simulation(2000)

#Truth
plot(data$x)
abline(v=data$tau,col=3,lty=1)
s <- c()
len <- diff(c(0,data$tau,length(data$x)))
for(i in seq_along(data$sig)) s <- c(s,rep(data$sig[i],len[i]))
lines(0+s*1.96,lty=1,col=3, type = "S")
lines(0-s*1.96,lty=1,col=3, type = "S")

#Estimated
plot(data$x)
abline(v=cpts(data$fit),col=2)
seg_sigs <- sqrt(param.est(data$fit)$variance)
overall_mean <- param.est(data$fit)$mean
len <- seg.len(data$fit)
s <- c()
for(i in seq_along(seg_sigs)) s <- c(s,rep(seg_sigs[i],len[i]))
lines(overall_mean +s*1.96,lty=1,col=2, type = "S")
lines(overall_mean -s*1.96,lty=1,col=2, type = "S")


#####Binary Segmentation, change in mean


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
  ##Need to use the ABSOLUTE values to detect potential new cpts.
  return(abs(Xt))
}


binseg <- function(s,e,thresh,X){

  #Return empty vector if hit a case where segment contains 1 datum
  #Need to return something in order for function recursion to work.
  #The STOP command will cause ALL operations to end!
  if(s >= e){
    cpt <- c()
    return(cpt)
  }

  stat <- cumsumStat(s,e,X)
  b_0 <- which.max(stat)
  #Note, b_0 is the index position between s & e, not the changepoint estimate
  if (stat[b_0] > thresh){
    cur_cpt <- b_0+s-1  #The detected changepoint value
    cpt_low <- binseg(s,cur_cpt,thresh,X)     #Check lower partion
    cpt_upp <- binseg((cur_cpt+1),e,thresh,X) #Check upper partition
    cpt <- c(cpt_low,cur_cpt,cpt_upp)
  }else{
    #If no further partioning is suggested, then return an empty vector
    #  so that the recursion does not break
    cpt <- c()
  }
  return(cpt)
}

##Application example

set.seed(3)
data <- c(rnorm(50,0,1),rnorm(50,2,1),rnorm(50,-2,1),rnorm(50,0,1),rnorm(50,3,1))
plot(data)

#Threshold: look at the manual page for ?changepoints from the wbs package
# To calculate the threshold, we need to estimate the standard deviation, sigma.
# This is done by using the median-absoulte-deviation estimate with the first order differences
# This approach is more robust than sd(data) as it takes account of known outliers across a changepoint event
sigma <- mad(diff(data)/sqrt(2))
threshold <- sigma*1.3*sqrt(2*log(length(data)))

cpts <- binseg(1,length(data),threshold,data)
abline(v=cpts,col=2)
abline(v=c(50,100,150,200), col=3,lty=2)


####Binary segmentation code in the "wbs" package

library(wbs)
cpts_sbs <- changepoints(sbs(data))
cpts_sbs 
