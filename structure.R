rm(list=ls())
##create the waveform for testing
x_L <- -3000
x_H <- 3000
xsamp <- 10^-2
X <- seq(x_L, x_H, by=xsamp)

#randomize the position of the periodic signals
vec_ran <- runif(n=100, min = x_L, max = x_H) ##select random center points for the periodic signals
a <- 1 #minimum decay sd for gaussian decay
b <- 5 #maximum decay sd for gaussian decay
gauss_decay <- runif(n=100, a, max=b) ##select random decay variables for the individual sinosoids

#create the final waveform
k <- 2*pi/0.3  ##frequency of the signal to implement
Y <- rep(0, times = length(X))
for (i in 1:length(vec_ran)) {
  Y <- Y + sin(k*(X + vec_ran[i]))*gauss(X, amp = 3, sig = gauss_decay[i], mu = vec_ran[i])
}
Y <- nrm(Y)
par(mfrow=c(1,1))
ClearPlot()
plot(X,Y, type = 'l', col=rgb(0,0,1,0.25))
abline(v=-1815, col='red')
abline(v=-1805, col='red')

ClearPlot()
plot(X,Y, type = 'l', col=rgb(0,0,1,0.25), xlim = c(-1815, -1805))
abline(v=-1815, col='red')
abline(v=-1805, col='red')
