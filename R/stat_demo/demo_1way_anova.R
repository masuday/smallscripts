# example of 1-way ANOVA
mu.0 <- rep(50,12)
mu.A <- c(rep(-10,6),rep(10,6))
A <- as.factor(c(rep("A1",6),rep("A2",6)))
y <- mu.0 + mu.A + rnorm(12,mean=0,sd=10)
result <- lm(y ~ A)
summary(result)
anova(result)

# parameters
mu <- 170
#mu.A1 <- -5
#mu.A2 <-  0
#mu.A3 <-  5
mu.A1 <-  0
mu.A2 <-  0
mu.A3 <-  0
v <- 81
stdev <- sqrt(v)

# one sample
n <- 8
mu.0 <- rep(mu,3*n)
mu.A <- c(rep(mu.A1,n),rep(mu.A2,n),rep(mu.A3,n))
e <- rnorm(3*n,mean=0,sd=stdev)
y <- mu.0 + mu.A + e

# many samples
p <- 3
n <- 8
N <- p*n
mu.0 <- replicate(10000,rep(mu,p*n))
mu.A <- replicate(10000,c(rep(mu.A1,n),rep(mu.A2,n),rep(mu.A3,n)))
e <- replicate(10000,rnorm(p*n,mean=0,sd=stdev))
y <- mu.0 + mu.A + e

# matrix notation
A <- as.factor(c(rep("A1",n),rep("A2",n),rep("A3",n)))
meany <- matrix(apply(y,2,mean) %x% rep(1,N), ncol=10000)
ym <- y - meany
X.0 <- matrix(rep(1,p*n))
X.A <- diag(p) %x% rep(1,n)
b <- solve(t(X.A) %*% X.A, t(X.A) %*% ym)
ssrm <- apply(ym*(X.A%*%b),2,sum)
sstm <- apply(ym*ym,2,sum)
sse <- sstm - ssrm

msrm <- ssrm/(p-1)
mse <- sse/(N-p)
f <- msrm/mse

plot.f <- function(x, df1, df2, breaks="Sturges"){
  h <- hist(x, plot=FALSE, breaks=breaks)
  plot(h, col="grey")
  m <- mean(x)
  s <- sd(x)
  xlines <-seq(min(h$breaks),max(h$breaks),length.out=100)
  lines(x = xlines,y=df(xlines,df1=df1,df2=df2) *length(x)*diff(h$breaks)[1])
}

plot.f(f,p-1,N-p, 40)

# ANOVA
result <- lm(y[,1] ~ 1 + A)
summary(result)
anova(result)
ssrm[1]
sse[1]
msrm[1]
mse[1]
