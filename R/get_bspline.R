# library(spline)

# returns B-spline coefficients (cubic; changed by degree=)
# range : 1 .. 7 (knot) .. 20

x.bs <- bs(x, Boundary.knots=c(1,20), knots=c(7))
