dx <- 0.001
x <- seq(-1,1,by=dx)
y <- 1.5*x^2
e_cdf <- cumsum(y*dx)
plot(x, e_cdf, ylab="CDF")
abline(h = 0.5, lty = 3)
abline(h = 0.25, lty = 3)
abline(h = 0.75, lty = 3)

q1 <- x[which(e_cdf >= 0.25)[1]]
q3 <- x[which(e_cdf >= 0.75)[1]]
median <- x[which(e_cdf >= 0.5)[1]]

mu <- sum(x*y*dx)
sigma <- sqrt(sum((x^2)*y*dx))
integral <- sum(y*dx)
#plot(x,y)
curve(1.5*x^2, from=-1, to=1, n=300, xlab="x", ylab="PDF", 
        col="blue", lwd=2, main="Plot of function"  )
points(x=c(q1,0,q3),y=c(0,0,0), pch=19)
