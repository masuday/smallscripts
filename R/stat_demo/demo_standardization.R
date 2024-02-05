# simulate normal deviates
y <- rnorm(100,mean=50,sd=10)
hist(y)

# statistics
mean(y)
var(y)
sd(y)
summary(y)

# the "true" standardization
z <- (y - 50)/10

mean(z)
var(z)
sd(z)
summary(z)

# a "practical" standardization
w <- (y - mean(y))/sd(y)

mean(w)
var(w)
sd(w)
summary(w)
