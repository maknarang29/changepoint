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
