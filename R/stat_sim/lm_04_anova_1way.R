#
# balanced 1-way anova model
# --------------------------
#
set.seed(123)
n <- 10
na <- 3
mu <- c(50,60,70)
ve <- 100
stde <- sqrt(ve)

y1 <- rnorm(n,mean=mu[1],sd=stde)
y2 <- rnorm(n,mean=mu[2],sd=stde)
y3 <- rnorm(n,mean=mu[3],sd=stde)
y <- c(y1,y2,y3)

a1 <- as.factor(rep("1",n))
a2 <- as.factor(rep("2",n))
a3 <- as.factor(rep("3",n))
a <- c(a1,a2,a3)

result <- lm(y ~ a)

result
summary(result)
anova(result)

#
# details
#
# some contrasts
options("contrasts")
model.matrix(~ a)
model.matrix(~ a, contrasts.arg = list(a = "contr.treatment"))
model.matrix(~ a, contrasts.arg = list(a = "contr.sum"))
model.matrix(~ a, contrasts.arg = list(a = "contr.SAS"))
model.matrix(~ a, contrasts.arg = list(a = "contr.helmert"))
model.matrix(~ a, contrasts.arg = list(a = "contr.poly"))
# no contrasts
model.matrix(~ a, contrasts.arg = list(a = contrasts(a, contrasts=FALSE)))

#
# normal equations with the default contrast
#
X <- model.matrix(~ a)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
# estimable function of expectation
b[1]+0
b[1]+b[2]
b[1]+b[3]

#
# normal equation with no contrasts
#
W <- model.matrix(~ a, contrasts.arg = list(a = contrasts(a, contrasts=FALSE)))
LHS.1 <- crossprod(W)
RHS.1 <- crossprod(W,y)
# Confirm the in failure b.1 <- solve(LHS,RHS)

# solution by QR decomposition
b.1 <- qr.coef(qr(LHS.1),RHS.1)
b.1[is.na(b.1)] <- 0

gensolve <- function(LHS,RHS){
   b <- qr.coef(qr(LHS),RHS)
   b[is.na(b)] <- 0
   return(b)
}

# estimable function of expectation
b.1[1] + b.1[2]
b.1[1] + b.1[3]
b.1[1] + b.1[4]

#
# ANOVA
#
# basic sum of squares
N <- length(y)
X0 <- model.matrix(y ~ 1)
SSM <- t(crossprod(X0,y))*solve(crossprod(X0),crossprod(X0,y))
# Confirm SSM = N*mean(y)^2

SST <- crossprod(y)
ym <- y - mean(y)
SSTm <- crossprod(ym)
# Confirm SSTm = SST - SSM

# sum of squares due to model (SSR)
X <- model.matrix(y ~ a)
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
dfr <- na - 1
dfe <- N - na
MSRm <- SSRm/dfr
MSE <- SSE/dfe
F <- MSRm/MSE
pval <- 1 - pf(F,dfr,dfe)

#
# data frame
#
df <- data.frame(y=y, x0=X0, a=a)
write.table(df, file="data_anova_1.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
