# library(mpoly)

# Arguments
#  x : original variable
#  n : degree of Legendre polynomials
#  x.min : lower bound of x (default = min(x))
#  x.max : upper bound of x (default = max(x))
#  dif : significant digits for output (default = no rounding)

legendre.matrix <- function(x, n, x.min=NULL, x.max=NULL, dig=NULL){
   legf <- as.function( legendre(0:n,normalized=TRUE) )
   if(is.null(x.min)){ xmin <- min(x) }
   else              { xmin <- x.min }
   if(is.null(x.max)){ xmax<- max(x) }
   else              { xmax <- x.max }
   #y <- -1 + 2*( (x - xmin)/(xmax-xmin) )
   y <- -1 + 2*( (x - min(xmin,xmax))/(max(xmin,xmax)-min(xmin,xmax)) )
   M <- legf(y)
   if(!is.null(dig)){
      M <- signif(legf(y),dig)
   }
   else{
      M <- legf(y)
   }
   return( M )
}
