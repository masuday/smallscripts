# initialization
set.seed(123456)

# family size
nm <- 3
nf <- 7
fsize <- nm + nf

# base animals (generation 0)
curr_gen <- 0
sex <- c(rep(1,nm),rep(2,nf))
sire <- rep(NA,fsize)
dam <- rep(NA,fsize)
gen <- rep(curr_gen,fsize)

#
# progeny (generation 1)
#
curr_gen <- curr_gen + 1

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


#
# progeny (generation 2)
#
curr_gen <- curr_gen + 1

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
