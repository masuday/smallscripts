# example of 1-way ANOVA
mu.0 <- rep(50,12)
mu.A <- c(rep(-10,6),rep(10,6))
A <- as.factor(c(rep("A1",6),rep("A2",6)))
y <- mu.0 + mu.A + rnorm(12,mean=0,sd=10)
result <- lm(y ~ A)
summary(result)
anova(result)
