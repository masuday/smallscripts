age_class <- function(parity,age, all=FALSE){
   if(parity==1){
      # (a,b]
      age_group <- c(18,20,21:30,32,35)
      offset <- 100
   }else if(parity==2){
      age_group <- c(30,32,33:44,46,50)
      offset <- 200
   }else if(parity==3){
      age_group <- c(42,45,46:56,58,60,65)
      offset <- 300
   }else{
      age_group <- c()
      offset <- 900
   }
   if(all){
      age_group <- c(-Inf,age_group,Inf)
   }
   ac <- cut(age, breaks=age_group, labels=FALSE) + offset
   return(ac)
}
