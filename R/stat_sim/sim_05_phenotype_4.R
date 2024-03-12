library(pedigreemm)

# initialization
set.seed(123456)

# mean and variance components
# y = mu + H + u + e
mu.y <- 50
nh <- 5
eff.H <- c(-20,-10,0,10,20)
vu <- 36
ve <- 64
sdu <- sqrt(vu)
sde <- sqrt(ve)

# family size
nm <- 3
nf <- 7
fsize <- nm + nf
maxgen <- 2

# base animals (generation 0)
curr_gen <- 0
sex <- c(rep(1,nm),rep(2,nf))
sire <- rep(NA,fsize)
dam <- rep(NA,fsize)
gen <- rep(curr_gen,fsize)
f <- rep(0,fsize)
u <- rnorm(fsize, mean=0, sd=sdu)
e <- rnorm(fsize, mean=0, sd=sde)
H <- as.factor(sample(1:nh, fsize, TRUE))
h <- eff.H[H]
y <- mu.y + h + u + e

# loop
for(curr_gen in 1:maxgen){
   # parents ID
   sire.id <- which(sex==1 & gen==curr_gen-1)
   dam.id <- which(sex==2 & gen==curr_gen-1)

   # progeny information
   sire.prog <- sample(sire.id, size=fsize, replace=TRUE)
   dam.prog <- sample(dam.id, size=fsize, replace=TRUE)
   sex.prog <- c(rep(1,nm),rep(2,nf))
   gen.prog <- rep(curr_gen,fsize)

   # phenotype
   fs <- f[sire.prog]
   fd <- f[dam.prog]
   sd.ms <-sqrt( (0.5 - 0.25*(fs+fd))*vu )
   ms <- rnorm(fsize, mean=0, sd=sd.ms)
   us <- u[sire.prog]
   ud <- u[dam.prog]
   pa <- 0.5*(us + ud)
   u.prog <- pa + ms
   e.prog <- rnorm(fsize, mean=0, sd=sde)
   H.prog <- as.factor(sample(1:nh, fsize, TRUE))
   h.prog <- eff.H[H.prog]
   y.prog <- mu.y + h.prog + u.prog + e.prog

   # update
   sire <- c(sire, sire.prog)
   dam <- c(dam, dam.prog)
   sex <- c(sex, sex.prog)
   gen <- c(gen, gen.prog)
   u <- c(u, u.prog)
   e <- c(e, e.prog)
   H <- c(H, H.prog)
   h <- c(h, h.prog)
   y <- c(y, y.prog)

   # inbreeding
   n <- length(gen)
   ped <- pedigree(label=1:n, sire=sire, dam=dam)
   f <- inbreeding(ped)
}
