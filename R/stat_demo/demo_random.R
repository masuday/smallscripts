# uniform random numbers
runif(1)
runif(2)
runif(3)

# seed
set.seed(123)
runif(5)
set.seed(123)
runif(5)

# range
runif(5, min=0, max=1)
runif(5, 0, 1)
runif(5, min=-1, max=1)
runif(5, min=0, max=100)

# histogram
x <- runif(10000, min=0, max=1)
hist(x)

# normal distribution
set.seed(123)
rnorm(1)
rnorm(5)
x <- rnorm(10000)
hist(x)

mean(x)
var(x)
sd(x)
summary(x)

# arbitrary mean and sd
x <- rnorm(10000, mean=50, sd=10)
hist(x)

mean(x)
var(x)
sd(x)
summary(x)

# standardization
z <- (x-50)/10
hist(z)

mean(z)
var(z)
sd(z)
summary(z)

# de-standardization
y <- 50 + z*10
mean(x - y)

# ------------------------

# simulate normal deviates
rnorm(1,mean=50,sd=10)
rnorm(3,mean=50,sd=10)

# different samplining
rnorm(5,mean=50,sd=10)
rnorm(5,mean=50,sd=10)
rnorm(5,mean=50,sd=10)

# visualize normal samples
y <- rnorm(100,mean=50,sd=10)
hist(y)

y <- rnorm(1000,mean=50,sd=10)
hist(y)

print( summary(y) )
