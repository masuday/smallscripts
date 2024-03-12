#
# variance component estimation
# -----------------------------
#
set.seed(123456)
N <- 40
nf <- 4
ns <- 10
vs <- 36
ve <- 64
stds <- sqrt(vs)
stde <- sqrt(ve)

# fixed effects
b <- c(50,60,70,80)
F <- as.factor(rep(1:nf,length.out=N))
X <- model.matrix(~ 0 + F)

# random effects
u <- rnorm(ns,mean=0,sd=stds)
S <- as.factor(sort(rep(1:ns,length.out=40)))
Z <- model.matrix(~ 0 + S)

# residual effects
e <- rnorm(N,mean=0,sd=stde)

# response
y <- X %*% b + Z %*% u + e

# computation using a package
# install.packages("lme4")
library(lme4)
result <- lmer(y ~ 0 + F + (1|S))

result
summary(result)

# BLUE
result@beta

# BLUP (scaled)
result@u * result@theta

#
# data frame
#
X0 <- model.matrix(y ~ 1)
df <- data.frame(y=y, x0=X0, F=F, S=S)
write.table(df, file="data_lmm_simplest.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
