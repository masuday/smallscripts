# initialization
set.seed(123456)

# number of animals
n <- 100
id <- 1:n

# observations
y <- rnorm(n,mean=50,sd=10)

# sorted index
idx <- order(y, decreasing=TRUE)
y[idx]
# Confirm: all( y[idx] == sort(y, decreasing=TRUE) )

# another method
#sorted.y <- sort(y, decreasing=TRUE, index.return=TRUE)
#y[sorted.y$ix]
#all(y[sorted.y$ix] == sorted.y$x)

# rankings: top 15%
p <- 0.15
t_pos <- n*0.15

# truncated point
t <- (y[idx])[t_pos]

# find index with values equal to or greater than t
selected.flag <- (y >= t)
selected.id <- id[selected.flag]

# equivalent expression
selected.id <- id[y >= t]
