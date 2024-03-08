#
# effect assignment
#

# -------------------------------------------
# regression
# x: independent variable
# n: sample size
n <- 10

# uniform distribution [1,5]
x <- round(runif(n,1,5))

# another method (better)
x <- sample(1:5, n, TRUE)

# general mean
mu <- 50

# b0, b1: pre-defined regression coefficients
b0 <- 1.5
b1 <- 2.0
m <- b0 + b1*x

# e: random error (normal distribution)
e <- rnorm(n, mean=0, sd=10)

# observation
y <- mu + m + e

# -------------------------------------------
# class effect
n <- 10
nlev <- 3
eff <- rnorm(nlev,mean=0,sd=10)

# periodic assignment 1
a <- as.factor(rep(1:nlev,length.out=n))

# periodic assignment 2
a <- as.factor(sort(rep(1:nlev,length.out=n)))

# random assignment
a <- as.factor(sample(1:nlev, n, TRUE))

# general mean
mu <- 50

# effect size
m <- eff[a]

# e: random error (normal distribution)
e <- rnorm(n, mean=0, sd=10)

# observation
y <- mu + m + e
