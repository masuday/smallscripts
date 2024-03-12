# initialization
set.seed(123456)

# sequential animal IDs
n <- 100
id <- 1:n

# random sampling of animals
m <- 10
selected.id <- sample(id, size=m)

# 15% of animals
p <- 0.15
m <- n*0.15
selected.id <- sample(id, size=m)

# sorted id
selected.id <- sort(sample(id, size=m))

# single TRUE or FALSE
r <- sample(c(TRUE,FALSE),size=1)

# random results
r <- sample(c(TRUE,FALSE),size=10,replace=TRUE)
table(r)

# sampling given probability
r <- sample(c(1,2,3),size=100,replace=TRUE,prob=c(0.5,0.4,0.1))
table(r)
