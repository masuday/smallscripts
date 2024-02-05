# normal distribution
x <- seq(170-9*4,170+9*4,0.1)
plot(x,dnorm(x,mean=170,sd=9),type="l")

# large samples
y <- rnorm(10000,mean=170,sd=9)
hist(y, 20)

# histogram and theoretical PDF
plot.normal <- function(y){
   h <- hist(y, plot=FALSE)
   plot(h)
   xlines <- seq(min(h$breaks),max(h$breaks),length.out=100)
   lines(x = xlines,y=dnorm(xlines,mean(y),sd(y)) *length(y)*diff(h$breaks)[1])
}
plot.normal(y)

# outlier
summary(y)
mean(y)
sd(y)
hist(y, 20)
plot(ecdf(y))

qqnorm(y)
qqline(y)
