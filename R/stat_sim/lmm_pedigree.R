# install.packages("pedigreemm")
library(pedigreemm)

# pedigree information
id   = c( 1, 2, 3, 4, 5, 6, 7)
sire = c(NA,NA, 1, 1, 3, 1, 5)
dam  = c(NA,NA,NA, 2, 4, 4, 6)

# pedigree object
ped <- pedigree(label=id, sire=sire, dam=dam)

# direct method
#ped <- pedigree(
#  label = c( 1, 2, 3, 4, 5, 6, 7),
#  sire  = c(NA,NA, 1, 1, 3, 1, 5),
#  dam   = c(NA,NA,NA, 2, 4, 4, 6)
#)

# inbreeding coefficients
f <- inbreeding(ped)
print(f)

# A-matrix and its inverse
A <- as.matrix(getA(ped))
Ainv <- as.matrix(getAInv(ped))

# updated pedigree
id   = c(id,   8, 9, 10)
sire = c(sire, 5, 7, 9)
dam  = c(dam,  6, 8, 8)
ped <- pedigree(label=id, sire=sire, dam=dam)
f <- inbreeding(ped)
print(f)
