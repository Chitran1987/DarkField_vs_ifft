#rm(list=ls())
##create the waveform for testing
X <- seq(-1000, 1000, by=10^-2)

#randomize the position of the periodic signals
vec_ran <- runif(n=100, min = -1000, max = 1000) ##select random center points for the periodic signals
a <- 0.1 #minimum decay sd for gaussian decay
b <- 1 #maximum decay sd for gaussian decay
gauss_decay <- runif(n=100, a, max=b) ##select random decay variables for the individual sinosoids

#create the final waveform
k <- 2*pi/0.3  ##frequency of the signal to implement
Y <- rep(0, times = length(X))
for (i in 1:length(vec_ran)) {
  Y <- Y + sin(k*X)*gauss(X, amp = 3, sig = gauss_decay[i], mu = vec_ran[i])
}