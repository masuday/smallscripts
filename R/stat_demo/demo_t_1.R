# parameters
mu <- 170
v <- 81
stdev <- sqrt(v)

# error of the average value
n <- 8
data <- replicate(10000,rnorm(n,mean=mu,sd=stdev))
smean <- apply(data, 2, mean)
ssd <- apply(data, 2, sd)

# normal distribution
plot.normal <- function(y, breaks="Sturges"){
   h <- hist(y, plot=FALSE, breaks=breaks)
   plot(h)
   xlines <- seq(min(h$breaks),max(h$breaks),length.out=100)
   lines(x = xlines,y=dnorm(xlines,mean(y),sd(y)) *length(y)*diff(h$breaks)[1])
}
plot.normal(smean)

# true "standardization" of smean
setrue <- stdev/sqrt(n)
stdsmean1 <- (smean - mu)/setrue
plot.normal(stdsmean1, 20)

# practical "standardization" of smean
se <- ssd/sqrt(n)
stdsmean2 <- (smean - mu)/se
plot.normal(stdsmean2, 20)

# t distribution
plot.t <- function(x, df, breaks="Sturges"){
  h <- hist(x, plot=FALSE, breaks=breaks)
  plot(h, col="grey")
  m <- mean(x)
  s <- sd(x)
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100)
  lines(x = xlines,y=dt(xlines,df=df) *length(x)*diff(h$breaks)[1])
}

plot.t(stdsmean2, n-1, 20)

# theoretical density
x <- seq(-5,5,0.01)
plot(x, dnorm(x), type="l")
curve(dt(x, df=7),-5,5, add=TRUE, col="blue")

# test sample mean compared with the true mean
n <- 8
y <- rnorm(n, mean=mu,sd=stdev)
mean(y)
se <- sd(y)/sqrt(n)
# or
# se <- sqrt(var(y)/n)

# 95% confidential interval
k <- qt(0.975, df=n-1)
mean(y) - k*se
mean(y) + k*se

# t-test
t <- (mean(y) - mu)/(sd(y)/sqrt(n))
p <- pt(t, df=n-1)*2

# single function for t-test
t.test(y, mu=170)
