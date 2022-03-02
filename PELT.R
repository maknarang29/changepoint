lossfunc <- function(x, s, t,sigma){
    stopifnot (t > s)
    data <- x[(s+1) : t]
    mu <- mean(data)
    n <- length(data)
    l1 <-  -(t-s)/2 * log( sigma**2 ) - sum((data - mu)**2)/(2*sigma**2) 
    l2 <-  -(n)/2 * log( sigma**2 ) - sum((data - mu)**2)/(2*sigma**2)
    return(l1)
}

costfunc <- function(x, s, t,sigma) {
    stopifnot(t > s);
    data <- x[(s+1) : t]
    summation <- sum(data)
    n <- length(data)
    c1 <- (t-s)*log(sigma) + ((sum( data**2 + (summation/(t-s))**2 - 2*data*summation/(t-s)))/(sigma**2)) 
    c2 <- (n)*log(sigma) + ((sum( data**2 + (summation/(n))**2 - 2*data*summation/(n)))/(sigma**2)) 
    return(c1)
}



costfunc_meanvar <- function(y,s,t){
    ys <- sum(y)
    l <- (t-s)
    C <- l*( log( sum( (y - (ys/l))**2 )) +1 )
    return(C)
 }

PELT<- function(data){
    beta <- floor(runif(1,1,10))
    K <- 0
     

}


F_ <- c()
cp_ <- c()
m_<- c()
for (t in 1:length(data)) {
    F_[t] <- F(s,t)
    cp_[t] <- cp(s,t)
    m_[t] <- m[cp[t]]+1

}


F <- function(s,t){
    if (t == 0){
        return 0
    }
    else{
        return(min(F(s) + costfunc_meanvar(data,s,t) + beta))
    }
}

cp <- function(s,t){
    if (t == 0){
        return 0
    }
    else{
        return(which.min(cp(s) + costfunc_meanvar(data,s,t) + beta))
    }
}


