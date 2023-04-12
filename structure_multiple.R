rm(list=ls())
##create the waveform for testing
x_L <- -3000
x_H <- 3000
xsamp <- 5*10^-3
X <- seq(x_L, x_H, by=xsamp)

#randomize the position of the periodic signals
vec_ran1 <- runif(n=100, min = x_L, max = x_H) ##select random center points for signal1
vec_ran2 <- runif(n=100, min = x_L, max = x_H) ##select random center points for signal2
vec_ran3 <- runif(n=100, min = x_L, max = x_H) ##select random center points for signal3

a <- 1 #minimum decay sd for gaussian decay
b <- 5 #maximum decay sd for gaussian decay
gauss_decay1 <- runif(n=100, min=a, max=b) ##select random decay variables for signal1
gauss_decay2 <- runif(n=100, min=a, max=b) ##select random decay variable for signal2
gauss_decay3 <- runif(n=100, min=a, max=b) ##select random decay variables for signal3

#create the final waveform
k1 <- 2*pi/0.3  ##frequency of signal1 
k2 <- 2*pi/0.7  ##frequency of signal2
k3 <- 2*pi/1.2  ##frequency of signal3
Y <- rep(0, times = length(X))
for (i in 1:length(vec_ran1)) {
  Y <- Y + sin(k1*(X + vec_ran1[i]))*gauss(X, amp = 3, sig = gauss_decay1[i], mu = vec_ran1[i])
  Y <- Y + sin(k2*(X + vec_ran2[i]))*gauss(X, amp = 3, sig = gauss_decay2[i], mu = vec_ran2[i])
  Y <- Y + sin(k3*(X + vec_ran3[i]))*gauss(X, amp = 3, sig = gauss_decay3[i], mu = vec_ran3[i])
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
