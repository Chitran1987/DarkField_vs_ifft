#rm(list=ls())
##create the waveform for testing
X <- seq(-1000, 1000, by=10^-4)
#randomize the position of the periodic signals
vec_ran <- runif(n=100, min = -1000, max = 1000) ##select random center points for the periodic signals
a <- 0.1 #minimum decay sd for gaussian decay
b <- 1 #maximum decay sd for gaussian decay
gauss_decay <- runif(n=100, a, max=b)
#create the final waveform
