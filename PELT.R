lossfunc <- function(x, s, t){
    mu <- mean(x);
    sig <- sqrt(var(x));
    stopifnot (t > s)
    l <-  -((t-s)/2) * log( sig**2 ) -  ( x[(s+1) : t]**2 + (t-s-1)*mu**2 + 2*mu*sum(x[s+1:t]))/(2*sigma**2) # nolint
    return(l)
}

costfunc <- function(x, s, t) {
    stopifnot(t > s);
    sig <- sqrt(var(x));
    summation <- sum(x[(s + 1) :t])
    c <- (t-s)*log( sig**2 ) + (sum(sum(x**2) + (summation/(t-s))**2 - 2*x*summation/(t-s))) / (sig**2) # nolint
    return(c)
}
costmeanvar <- function # nolint