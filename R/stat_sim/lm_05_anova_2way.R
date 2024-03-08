#
# balanced 2-way anova model
# ------------------------------
#
set.seed(12345)
N <- 30
n <- 10
na <- 3
nb <- 2
mu <- 0
eff.A <- c(50,60,70)
eff.B <- c(0,0)
ve <- 100
stde <- sqrt(ve)

A <- as.factor(sort(rep(1:na,length.out=N)))
B <- as.factor(rep(1:nb,length.out=N))
m <- eff.A[A] + eff.B[B]
e <- rnorm(N,mean=0,sd=stde)

y <- mu + m + e

result <- lm(y ~ A + B)

result
summary(result)
anova(result)

#
# details
#
# some contrasts
options("contrasts")
#model.matrix(~ a, contrasts.arg = list(a = "contr.sum"))

#
# normal equations with the default contrast
#
X <- model.matrix(~ A + B)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
# estimable function of expectation
b[1]+0+0        # mu + A1 + B1
b[1]+b[2]+b[4]  # mu + A2 + B2

#
# ANOVA (overall)
#
# basic sum of squares
SST <- crossprod(y)
ym <- y - mean(y)
SSTm <- crossprod(ym)

X0 <- model.matrix(y ~ 1)
SSM <- t(crossprod(X0,y))*solve(crossprod(X0),crossprod(X0,y))

# sum of squares due to model (SSR)
X <- model.matrix(y ~ A + B)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
yhat <- X %*% b
SSR <- crossprod(yhat)
SSRm <- SSR - SSM

# sum of squares due to errors (residuals)
e <- y - yhat
SSE <- crossprod(e)

# overall ANOVA table
dfr <- na - 1
dfe <- N - na
MSRm <- SSRm/dfr
MSE <- SSE/dfe
F <- MSRm/MSE
pval <- 1 - pf(F,dfr,dfe)

#
# ANOVA (indiviaual effect)
# for balanced data
#
# skip computing details
result.A <- lm(y ~ A)
SSRm.A <- anova(result.A)[1,2]
result.B <- lm(y ~ B)
SSRm.B <- anova(result.B)[1,2]
# Confirm SSRm = SSRm.A + SSRm.B

# standard error
vehat <- MSE[1,1]
se <- sqrt(diag(solve(crossprod(X)) * vehat))
tval <- b/se
pval <- 2*(1 - pt(tval,N-qr(X)$rank))

#
# data frame
#
df <- data.frame(y=y, x0=X0, A=A, B=B)
write.table(df, file="data_anova_2.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
