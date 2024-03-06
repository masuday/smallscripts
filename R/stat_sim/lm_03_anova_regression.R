#
# regression model
# --------------------------
#
set.seed(123)
n <- 18
b0 <- 20
b1 <- 8
ve <- 16
stde <- sqrt(ve)

x <- round(runif(n,1,10))
e <- rnorm(n,mean=0,sd=stde)
y <- b0 + b1*x + e
# Confirm the linear trend between x and y as plot(x,y)

result <- lm(y ~ x)

result
summary(result)
anova(result)
# Confirm the regression line (prediction) with abline(result)

#
# normal equations
#
X <- model.matrix(~ x)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
b

#
# ANOVA
#
# basic sum of squares
N <- length(y)
X0 <- rep(1,n)
SSM <- t(crossprod(X0,y)) %*% solve(crossprod(X0),crossprod(X0,y))
# Confirm SSM = N*mean(y)^2

SST <- crossprod(y)
ym <- y - mean(y)
SSTm <- crossprod(ym)
# Confirm SSTm = SST - SSM

# sum of squares due to model (SSR)
X <- model.matrix(y ~ x)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
yhat <- X %*% b
SSR <- crossprod(yhat)
SSRm <- SSR - SSM
# Confirm: SSRm is available from the normal equations with ym.
# bm <- solve(LHS,crossprod(X,ym))
# ymhat <- X %*% bm
# crossprod(ymhat)

# sum of squares due to errors (residuals)
e <- y - yhat
SSE <- crossprod(e)
# Confirm: SSE = crossprod(ym-ymhat)
# Confirm: SST = SSM + SSRm + SSE, or SSTm = SSRm + SSE

# ANOVA table
dfr <- qr(X)$rank - 1
dfe <- N - qr(X)$rank
MSRm <- SSRm/dfr
MSE <- SSE/dfe
F <- MSRm/MSE
pval <- 1 - pf(F,dfr,dfe)

# coefficient of determination (R^2)
R2 <- SSRm/SSTm
# Confirm plots: plot(x,y); points(x,yhat,col="blue",pch=19)
# Confirm R2 = cor(y,yhat)^2 in this case.
# Note: It is the case for a single regression model.

# standard error
vehat <- MSE[1,1]
se <- sqrt(diag(solve(crossprod(X)) * vehat))
tval <- b/se
pval <- 2*(1 - pt(tval,N-qr(X)$rank))

#
# data frame
#
df <- data.frame(y=y, x0=X0, x=x)
write.table(df, file="data_reg.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
