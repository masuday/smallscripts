#
# unbalanced 2-way anova model
# ------------------------------
#
# install.packages("car")
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

# unbalanced A
A <- as.factor(sort(rep(1:na,length.out=N)))
B <- as.factor(rep(1:nb,length.out=N))
A[1] <- 2
m <- eff.A[A] + eff.B[B]
e <- rnorm(N,mean=0,sd=stde)

y <- mu + m + e

result <- lm(y ~ A + B)

result
summary(result)
anova(result)

# SSR ~ B + A
anova(lm(y ~ B + A))

#
# details
#
# some contrasts
options("contrasts")
#model.matrix(~ a, contrasts.arg = list(a = "contr.sum"))

# reduction of SS: SSRm = R(...)
# R(mu,B|A) = R(mu,A,B) - R(mu,B)
# default: options(contrasts = c("contr.treatment","contr.poly"))
get_ssr <- function(m){
   contr.orig <- options("contrasts")
   options(contrasts = c("contr.sum","contr.poly"))
   result <- lm(m)
   X <- model.matrix(m)
   y <- result$model$y
   b <- solve(crossprod(X)) %*% crossprod(X,y)
   yhat <- X %*% b
   SSR <- crossprod(yhat)
   options(contr.orig)
   return(SSR)
}

# Type I SS: successive reduction of SS
#  1) y ~ mu
#  2) y ~ mu + A
#  3) y ~ mu + A + B
anova(lm(y ~ A + B))
SSRm1.mu1 <- get_ssr(y ~ 1)
SSRm1.A1 <- get_ssr(y ~ 1 + A) - SSRm1.mu1
SSRm1.B1 <- get_ssr(y ~ 1 + A + B) - SSRm1.A1 - SSRm1.mu1

anova(lm(y ~ B + A))
SSRm1.mu2 <- get_ssr(y ~ 1)
SSRm1.B2 <- get_ssr(y ~ 1 + B) - SSRm1.mu2
SSRm1.A2 <- get_ssr(y ~ 1 + B + A) - SSRm1.B2 - SSRm1.mu2


# Type III SS: full model reduction
# NOTE: See Searle (1987) with interaction effects
# SSRm.A: R(mu,B|A) = R(mu,A,B) - R(mu,B)
# SSRm.B: R(mu,A|B) = R(mu,A,B) - R(mu,A)
SSRm3.A <- get_ssr(y ~ A + B) - get_ssr(y ~ B)
SSRm3.B <- get_ssr(y ~ A + B) - get_ssr(y ~ A)

# library(car)
#anova_type3 <- function(m){
#   contr.orig <- options("contrasts")
#   options(contrasts = c("contr.sum","contr.poly"))
#   print( Anova(lm(m), type=3) )
#   options(contr.orig)
#}
#anova_type3(y ~ A + B)

# standard error
X0 <- model.matrix(y ~ 1)
X <- model.matrix(~ A + B)
LHS <- crossprod(X)
RHS <- crossprod(X,y)
b <- solve(LHS,RHS)
result <- lm(y ~ A + B)
ehat <- y - predict(result)
sse <- crossprod(ehat)
MSE <- sse/(N-qr(result)$rank)
vehat <- MSE[1,1]
se <- sqrt(diag(solve(crossprod(X)) * vehat))
tval <- b/se
pval <- 2*(1 - pt(tval,N-qr(result)$rank))

#
# data frame
#
df <- data.frame(y=y, x0=X0, A=A, B=B)
write.table(df, file="data_anova_2_unbalanced.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
