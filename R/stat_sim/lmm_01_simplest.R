#
# simplest linear mixed model
# ---------------------------
#
set.seed(123456)
N <- 40
na <- 4
ns <- 10
vs <- 36
ve <- 64
stds <- sqrt(vs)
stde <- sqrt(ve)

# fixed effects
b <- c(50,60,70,80)
A <- as.factor(rep(1:na,length.out=N))
X <- model.matrix(~ 0 + A)

# random effects
u <- rnorm(ns,mean=0,sd=stds)
S <- as.factor(sort(rep(1:ns,length.out=40)))
Z <- model.matrix(~ 0 + S)

# residual effects
e <- rnorm(N,mean=0,sd=stde)

# response
y <- X %*% b + Z %*% u + e

#
# simplified mixed model equations (MME)
#
lambda <- ve/vs
LHS <- rbind(
  cbind(crossprod(X,X), crossprod(X,Z)),
  cbind(crossprod(Z,X), crossprod(Z,Z)+diag(ns)*lambda)
)
RHS <- rbind(
  crossprod(X,y),
  crossprod(Z,y)
)
sol <- solve(LHS,RHS)

b.hat <- sol[1:na]
u.hat <- sol[(na+1):(na+ns)]

# SE for BLUE and PEV for BLUP
sepev <- sqrt(diag(solve(LHS))*ve)
se <- sepev[1:na]
pev <- sepev[(na+1):(na+ns)]

#
# full MME
#
G <- diag(ns)*vs
R <- diag(N)*ve
Ginv <- solve(G)
Rinv <- solve(R)
LHS.f <- rbind(
  cbind(t(X) %*% Rinv %*% X, t(X) %*% Rinv %*% Z),
  cbind(t(Z) %*% Rinv %*% X, t(Z) %*% Rinv %*% Z + Ginv)
)
RHS.f <- rbind(
  t(X) %*% Rinv %*%y,
  t(Z) %*% Rinv %*% y
)
sol.f <- solve(LHS.f,RHS.f)
# Confirm: sol.f - sol = 0

# SE for BLUE and PEV for BLUP
sepev.f <- sqrt(diag(solve(LHS.f)))
# Confirm: sepev.f - sepev.f = 0

#
# literal solutions
#
V <- tcrossprod(Z %*% G,Z) + R
Vinv <- solve(V)

# BLUE = GLS
LHS.gls <- t(X) %*% Vinv %*% X
RHS.gls <- t(X) %*% Vinv %*% y
b.gls <- solve(LHS.gls, RHS.gls)
# Confirm: b.gls - b.hat = 0

# BLUP
u.blup <- tcrossprod(G,Z) %*% Vinv %*% (y - X %*% b.gls)
# Confirm: u.blup - u.hat = 0

#
# data frame
#
X0 <- model.matrix(y ~ 1)
df <- data.frame(y=y, x0=X0, A=A, S=S)
write.table(df, file="data_lmm_simplest.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
