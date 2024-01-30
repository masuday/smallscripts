# example of principal component analysis and cluster analysis
group <- c(rep(1,5),rep(2,5),rep(3,5))
g1 <- t(replicate(5,sample(c(0,1,2),size=20,replace=TRUE,prob=c(0.1,0.1,0.8))))
g2 <- t(replicate(5,sample(c(0,1,2),size=20,replace=TRUE,prob=c(0.1,0.8,0.1))))
g3 <- t(replicate(5,sample(c(0,1,2),size=20,replace=TRUE,prob=c(0.8,0.1,0.1))))
gdata <- rbind(g1,g2,g3)
pc <- prcomp(gdata)
plot(pc$x[,1:2], col=c("black","red","blue")[group])
plot(hclust(dist(gdata)))
