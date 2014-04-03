# theory
i <- seq(from=.01,to=100,by=.01)
x <- matrix(rep(i,5),ncol=5)
k <- matrix(rep(1:5,by=.5,each=10000),ncol=5)
b <- .95
y <- exp((1/b)*(k-log(x)))
matplot(x,y, xlim=c(0,15), ylim=c(0,15),type="l"
        , main = "indifference"
        , xlab = "consumption at t"
        , ylab= "consumption at t+1")
# derivative abstract calc
D(expression(exp((1/b)*(k-log(x)))),"x")
fxn <- function(x) {log(x)+.95*log(x)-i}
rootpts <- vector()
# calculate points where ct = ct+1
for (i in 1:5) {rootpts[i] <- uniroot(fxn,c(0,100))$root}
points(rootpts,rootpts,pch=20)
# this is the derivative at rootpt, given the above D() fxn
m <- -(exp((1/b) * (1:5 - log(rootpts))) * ((1/b) * (1/rootpts)))
rootints <- rootpts - m*rootpts
#plotting
for (i in 1:5) {abline(a=rootints[i],b=m[i],col=i)}
#edit check
