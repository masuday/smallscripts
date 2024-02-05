# a set of realized values
mu <- 50
v <- 100
stdev <- sqrt(v)
n <- 10

# error of the average value
n <- 10
data <- replicate(10000,rnorm(n,mean=mu,sd=stdev))
smean <- apply(data, 2, mean)
ssd <- apply(data, 2, sd)

# unbiasedness for mean and sd
mean(smean)
sd(smean)

mean(ssd)
sd(ssd)

# biased sd
sd.biased <- function(x){
   n <- length(x)
   s <- sd(x)*(n-1)/n
   return(s)
}

ssd.biased <- apply(data, 2, sd.biased)
mean(ssd.biased)
sd(ssd.biased)
