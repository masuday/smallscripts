# a set of realized values
mu <- 50
v <- 100
stdev <- sqrt(v)
n <- 10

# different average from the same parameters
x <- rnorm(n, mean=mu, sd=stdev)
mean(x)

x <- rnorm(n, mean=mu, sd=stdev)
mean(x)

# error of the average value
n <- 10
data <- replicate(10000,rnorm(n,mean=mu,sd=stdev))
smean <- apply(data, 2, mean)

plot.normal <- function(y){
   h <- hist(y, plot=FALSE)
   plot(h)
   xlines <- seq(min(h$breaks),max(h$breaks),length.out=100)
   lines(x = xlines,y=dnorm(xlines,mean(y),sd(y)) *length(y)*diff(h$breaks)[1])
}

mean(smean)
var(smean)
hist(smean)
plot.normal(smean)
