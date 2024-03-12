library(pedigreemm)

# reading the script in the same directory
source("sim_05_phenotype_4.R")

# all phenotypes
flag <- rep(TRUE,n)

# selected phenotypes
# limited generation
#   flag <- gen %in% c(0,1)
# female only
#   flag <- sex==2
# combined condition
#   flag <- (gen %in% c(0,1)) & (sex==2)

#
# data frame
#
blupf90_pedigree <- function(ped,f){
   n <- length(ped@label)
   aid <- c()
   sid <- c()
   did <- c()
   inbcode <- c()
   for(i in 1:n){
      aid <- c(aid,i)
      sid <- c(sid,ifelse(is.na(ped@sire[i]),0,ped@sire[i]))
      did <- c(did,ifelse(is.na(ped@dam[i]),0,ped@dam[i]))
      ms <- ifelse(is.na(ped@sire[i]),1,0)
      md <- ifelse(is.na(ped@dam[i]),1,0)
      fs <- ifelse(is.na(ped@sire[i]),0,f[ped@sire[i]])
      fd <- ifelse(is.na(ped@dam[i]),0,f[ped@dam[i]])
      b <- round(4000/((1+ms)*(1-fs)+(1+md)*(1-fd)))
      inbcode <- c(inbcode,b)
   }
   df <- data.frame(aid=aid,sid=sid,did=did,inbcode=inbcode,f=f)
   return(df)
}

X0 <- model.matrix(y ~ 1)
df <- data.frame(y=y, x0=X0, H=H, gen=gen, sex=sex, id=1:n, u=u, e=e)
write.table(df[flag,], file="data.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
print("data: y, 1, herd, gen, sex, ID, u, e")

df.ped <- blupf90_pedigree(ped,f)
write.table(df.ped, file="ped.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
print("ped : id, sire, dam, inb/upg code, inbcoef")
