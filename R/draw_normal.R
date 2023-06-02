# normal distribution
x <- seq(-3,+3,by=0.01)
y <- dnorm(x)
plot(x,y, type="l", lwd=3, bty="n", axes=F, xlab="", ylab="")
#axis(side=1, labels=FALSE, lwd.tick=0, lwd=2)
lines(c(-3,3),c(0,0),lwd=2)

# filled
plot.new()
lower.x <- 1.5
upper.x <- 3.0
cord.x <- c(lower.x,seq(lower.x,upper.x,0.01),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,0.01)),0)
plot(x,y, type="l", lwd=3, bty="n", axes=F, xlab="", ylab="")
lines(c(-3,3),c(0,0),lwd=2)
polygon(cord.x,cord.y,col='skyblue')
