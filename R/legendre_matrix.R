# library(mpoly)

legendre.matrix <- function(x, n, dig=NULL){
   legf <- as.function( legendre(0:n,normalized=TRUE) )
   x.min <- min(x)
   x.max <- max(x)
   y <- -1 + 2*( (x - x.min)/(x.max-x.min) )
   M <- legf(y)
   if(!is.null(dig)){
      M <- signif(legf(y),dig)
   }
   else{
      M <- legf(y)
   }
   return( M )
}
