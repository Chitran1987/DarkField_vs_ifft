rm(list=ls())
##create the waveform for testing
x_L <- -3000
x_H <- 3000
xsamp <- 10^-1
X <- seq(x_L, x_H, by=xsamp)

#randomize the position of the periodic signals
vec_ran <- runif(n=100, min = x_L, max = x_H) ##select random center points for the periodic signals
a <- 0.1 #minimum decay sd for gaussian decay
b <- 1 #maximum decay sd for gaussian decay
gauss_decay <- runif(n=100, a, max=b) ##select random decay variables for the individual sinosoids

#create the final waveform
k <- 2*pi/0.3  ##frequency of the signal to implement
Y <- rep(0, times = length(X))
for (i in 1:length(vec_ran)) {
  Y <- Y + sin(k*(X + vec_ran[i]))*gauss(X, amp = 3, sig = gauss_decay[i], mu = vec_ran[i])
}
