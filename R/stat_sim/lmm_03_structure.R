#
# linear mixed model with covariance structure
# --------------------------------------------
#
set.seed(123456)
N <- 5
nf <- 2
ns <- 10
vu <- 36
ve <- 64
stdu <- sqrt(vu)
stde <- sqrt(ve)

# fixed effects
b <- c(50,60)
F <- as.factor(rep(1:nf,length.out=N))
X <- model.matrix(~ 0 + F)

# pedigree information
#id   = c( 1, 2, 3, 4, 5, 6, 7)
#sire = c(NA,NA, 1, 1, 3, 1, 5)
#dam  = c(NA,NA,NA, 2, 4, 4, 6)

# numerator relationship matrix
a.value <- c(
  1.000, 0.00, 0.500, 0.5000, 0.50000, 0.75000, 0.62500,
  0.000, 1.00, 0.000, 0.5000, 0.25000, 0.25000, 0.25000,
  0.500, 0.00, 1.000, 0.2500, 0.62500, 0.37500, 0.50000,
  0.500, 0.50, 0.250, 1.0000, 0.62500, 0.75000, 0.68750,
  0.500, 0.25, 0.625, 0.6250, 1.12500, 0.56250, 0.84375,
  0.750, 0.25, 0.375, 0.7500, 0.56250, 1.25000, 0.90625,
  0.625, 0.25, 0.500, 0.6875, 0.84375, 0.90625, 1.28125
)
A <- matrix(a.value, ncol=7)
ns <- ncol(A)

# random effects
# phenotypes only for animals 3 - 7
# u = chol(A) * z * stdu
u <- t(chol(A)) %*% rnorm(ns,mean=0,sd=1)*stdu
S <- as.factor(3:7)
Z <- cbind(matrix(0,nrow=5,ncol=2), diag(5))

# residual effects
e <- rnorm(N,mean=0,sd=stde)

# response
y <- X %*% b + Z %*% u + e

#
# simplified mixed model equations (MME)
#
Ainv <- solve(A)
lambda <- ve/vu
LHS <- rbind(
  cbind(crossprod(X,X), crossprod(X,Z)),
  cbind(crossprod(Z,X), crossprod(Z,Z)+Ainv*lambda)
)
RHS <- rbind(
  crossprod(X,y),
  crossprod(Z,y)
)
sol <- solve(LHS,RHS)
sol

b.hat <- sol[1:nf]
u.hat <- sol[(nf+1):(nf+ns)]

# SE for BLUE and PEV for BLUP
sepev <- sqrt(diag(solve(LHS))*ve)
se <- sepev[1:nf]
pev <- sepev[(nf+1):(nf+ns)]

#
# data frame
#
triplet_matrix <- function(A){
   nc <- ncol(A)
   ivec <- c()
   jvec <- c()
   avec <- c()
   for(j in 1:nc){
      for(i in j:nc){
         ivec <- c(ivec,i)
         jvec <- c(jvec,j)
         avec <- c(avec,A[i,j])
      }
   }
   df <- data.frame(i=ivec,j=jvec,a=avec)
   return(df)
}

X0 <- model.matrix(y ~ 1)
df <- data.frame(y=y, x0=X0, F=F, S=S)
write.table(df, file="data_lmm_structure.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
df.ainv <- triplet_matrix(Ainv)
write.table(df.ainv, file="user_file_lmm_structure.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
