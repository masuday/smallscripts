# parameters
mu1 <- 170
mu2 <- 174
v <- 81
stdev <- sqrt(v)

# pooled variance
var.pooled <- function(...){
  p <- list(...)
  df <- 0
  ss <- 0
  for(x in p){
    if(is.vector(x) & is.numeric(x)){
      n <- length(x)
      ss <- ss + (n-1)*var(x)
      df <- df + (n-1)
    }
  }
  v <- ss/df
  return(v)
}

# pooled standard deviation
sd.pooled <- function(...){
  return(sqrt(var.pooled(...)))
}

# pooled variance and standard deviation for two matrices
var.pooled2 <- function(m1,m2){
  k <- ncol(m1)
  v <- c()
  for(j in 1:k){
    v <- c(v, var.pooled(m1[,j],m2[,j]))
  }
  return(v)
}

sd.pooled2 <- function(m1,m2){
  return(sqrt(var.pooled(...)))
}

# t distribution
plot.t <- function(x, df, breaks="Sturges"){
  h <- hist(x, plot=FALSE, breaks=breaks)
  plot(h, col="grey")
  m <- mean(x)
  s <- sd(x)
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100)
  lines(x = xlines,y=dt(xlines,df=df) *length(x)*diff(h$breaks)[1])
}

# two samples
set.seed(23407)
n <- 8
y1 <- rnorm(n, mean=mu1, sd=stdev)
y2 <- rnorm(n, mean=mu2, sd=stdev)
mean(y1)
mean(y2)
sd.pooled(y1,y2)

# error of the average value
data1 <- replicate(10000,rnorm(n,mean=mu1,sd=stdev))
data2 <- replicate(10000,rnorm(n,mean=mu1,sd=stdev))
smean1 <- apply(data1, 2, mean)
smean2 <- apply(data2, 2, mean)
svar <- var.pooled2(data1,data2)
se <- sqrt(svar/n + svar/n)
t <- (smean1-smean2)/se
hist(t,40)
plot.t(t,2*n-2,40)

# t-test for two sampls
y1 <- rnorm(n, mean=mu1, sd=stdev)
y2 <- rnorm(n, mean=mu2, sd=stdev)
mean(y1)
mean(y2)
se <- sqrt(var.pooled(y1,y2)/n + var.pooled(y1,y2)/n)
t <- (mean(y1)-mean(y2))/se
pt(t,2*n-2)*2

# t-test
t.test(y1,y2, var.equal=TRUE)
