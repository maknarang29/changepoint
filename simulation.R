library(EnvCpt)

## Normal distribution with varied mean
x1 <- rnorm(100,mean = (1:100)/20, 1)
x2 <- rnorm(100, mean = (100:1)/20,1)
x <- c(x1,x2)
plot(x)

## Normal Distribution with varied mean and variance

x1 <- rnorm(100,mean = (1:100)/20, sd = abs(rlnorm(100)))
x2 <- rnorm(100,mean = (100:1)/20,sd = abs(rlnorm(100)))
x3 <- rnorm(100,mean = 2*(1:100)/40, sd = abs(rlnorm(100)))
x4 <- rnorm(100,mean = 2*(100:1)/40,sd = abs(rlnorm(100)))
x <- c(x1,x2,x3,x4)
plot(x)




fit <- envcpt(data = x, models = "trendcpt", verbose = FALSE)
fit$trendcpt
ncpts(fit$trendcpt)
plot(fit$trendcpt)
param.est(fit$trendcpt)

## Using t distribution
x1 <- rt(100,4,1.5*seq(1,100)+runif(100))
x2 <- rt(100,4,1.5*seq(100,1)+runif(100))
X <- c(x1,x2)
plot(X)
fit <- envcpt(data = X, models = "trendcpt", verbose = FALSE)
plot(fit$trendcpt)



## Using Poisson Distribution

x1 <- rpois(100,1.5*(1:100)**2)+rnorm(100)**ceiling(runif(1,1,15))
x2 <- rpois(100,1.5*(100:1)**2)+rnorm(100)**ceiling(runif(1,1,15))
x <- c(x1,x2)
plot(x)



d <- 1
m_est <- rep(NA,length = 100)
tau_est <- list()
for(i in 1:length(m_est)){
  set.seed(i)
  x <- c(rnorm(100,mean=-d,sd=1),rnorm(100,mean=d,sd=1))
  fit <- cpt.mean(x,method = "PELT")
  m_est[i] <- ncpts(fit)
  tau_est[[i]] <- cpts(fit)
}
table(m_est)
table(unlist(tau_est[m_est == 1]))
barplot(table(c(unlist(tau_est[m_est == 1]),1:length(x)))-1)



unlist(m_est)
