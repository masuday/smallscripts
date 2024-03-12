library(pedigreemm)

# initialization
set.seed(123456)

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

   # update
   sire <- c(sire, sire.prog)
   dam <- c(dam, dam.prog)
   sex <- c(sex, sex.prog)
   gen <- c(gen, gen.prog)

   # inbreeding
   n <- length(gen)
   ped <- pedigree(label=1:n, sire=sire, dam=dam)
   f <- inbreeding(ped)
}
