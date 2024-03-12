#
# linear mixed model with covariance structure
# --------------------------------------------
library(pedigreemm)
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
id   = c( 1, 2, 3, 4, 5, 6, 7)
sire = c(NA,NA, 1, 1, 3, 1, 5)
dam  = c(NA,NA,NA, 2, 4, 4, 6)
ped <- pedigree(label=id, sire=sire, dam=dam)
f <- inbreeding(ped)

# numerator relationship matrix and its inverse
A <- as.matrix(getA(ped))
Ainv <- as.matrix(getAInv(ped))
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
blupf90_pedigree <- function(ped,f){
   n <- length(ped@label)
   aid <- c()
   sid <- c()
   did <- c()
   inbcode <- c()
   for(i in 1:n){
      aid <- c(aid,i)
      sid <- c(sid,ifelse(is.na(ped@sire[i]),0,ped@sire[i]))
      did <- c(did,ifelse(is.na(ped@dam[i]),0,ped@dam[i]))
      ms <- ifelse(is.na(ped@sire[i]),1,0)
      md <- ifelse(is.na(ped@dam[i]),1,0)
      fs <- ifelse(is.na(ped@sire[i]),0,f[ped@sire[i]])
      fd <- ifelse(is.na(ped@dam[i]),0,f[ped@dam[i]])
      b <- round(4000/((1+ms)*(1-fs)+(1+md)*(1-fd)))
      inbcode <- c(inbcode,b)
   }
   df <- data.frame(aid=aid,sid=sid,did=did,inbcode=inbcode)
   return(df)
}

X0 <- model.matrix(y ~ 1)
df <- data.frame(y=y, x0=X0, F=F, S=S)
write.table(df, file="data_lmm_animal.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
df.ped <- blupf90_pedigree(ped,f)
write.table(df.ped, file="ped_lmm_animal.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
