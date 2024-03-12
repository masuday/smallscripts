# initialization
set.seed(123456)

# mean and variance components
mu <- 50
ve <- 64
sde <- sqrt(ve)

# single animal
# y = mu + e
e <- rnorm(1, mean=0, sd=sde)
y <- mu + e

# multiple animals
e <- rnorm(3, mean=0, sd=sde)
y <- mu + e

# multiple animals with different variance
e <- rnorm(3, mean=0, sd=c(sde,10*sde,100*sde))
y <- mu + e

# ----------------------------------------

# breeding value generator
varu <- 36
sdu <- sqrt(varu)

# parent breeding value and inbreeding
u <- rnorm(2,mean=0,sd=sdu)
f <- c(0.02,0.05)

# ID
sire <- 1
dam <- 2

# progeny breeding value
# u = parent_average (PA) + Mendelian_sampling_effect (MS)
var.ms <- (0.5 - 0.25*(f[sire]+f[dam]))*varu
sd.ms <- sqrt(var.ms)
ms <- rnorm(1,mean=0,sd=sd.ms)
pa <- 0.5*(u[sire]+u[dam])
u.prog <- pa + ms

# phenotype
e.prog <- rnorm(1,mean=0,sd=sde)
y.prog <- mu + u.prog + e.prog


# multiple animals
sire <- c(1,2,3)
dam <- c(4,5,6)
u <- rnorm(6,mean=0,sd=sdu)
f <- c(0.02,0.05,0.04,0.02,0.00,0.03)
var.ms <- (0.5 - 0.25*(f[sire]+f[dam]))*varu
sd.ms <- sqrt(var.ms)
ms <- rnorm(3,mean=0,sd=sd.ms)
pa <- 0.5*(u[sire]+u[dam])
u.prog <- pa + ms
e.prog <- rnorm(3,mean=0,sd=sde)
y.prog <- mu + u.prog + e.prog
