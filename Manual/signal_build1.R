rm(list=ls())
library('StatsChitran') #Call library
ClearPlot() #Clear the plot
t <- seq(0,10, by=1/60000) #define the time axis
fcomp <- c(1000, 5000, 7734) #set which frequencies to build
Y <- 3*sin(2*pi*fcomp[1]*t)*gauss(t, amp =5, sig = 0.7, mu=1 ) + 5*sin(2*pi*fcomp[2]*t)*gauss(t, amp = 4, sig = 1, mu=3) + 4*sin(2*pi*fcomp[3]*t)*gauss(t, amp = 4.3, sig = 3, mu=7) #define signal
Y <- Y + runif(length(Y), min = 0.25*min(Y), max = 0.25*max(Y)) #add noise
plot(t,Y, col=rgb(0,0,1,0.25), typ='l')
fY <- ft(t,Y,w=T) #function available with StatsChitran
plot(fY$wf, abs(fY$fy), col= 'grey', typ='l', xlab='w', ylab='Amp')

##Plotting the frequency windows
fY <- ft(t,Y,w=T) #function available with StatsChitran
plot(fY$wf, abs(fY$fy), col= 'grey', typ='l', xlab='w', ylab='Amp')

##for the 1000Hz freq
abline(v=2*pi*800, col='red')
abline(v=2*pi*1200, col='red')

##for the 5000Hz freq
abline(v=2*pi*4800, col='blue')
abline(v=2*pi*5200, col='blue')

##for the 7734Hz freq
abline(v=2*pi*7534, col='green')
abline(v=2*pi*7934, col='green')

##building the datasets for freq_map()
#build dataframe for w_int (Integration window)
w1 <- c(2*pi*800, 2*pi*1200)
w2 <- c(2*pi*4800, 2*pi*5200)
w3 <- c(2*pi*7534, 2*pi*7934)
w_dat <- data.frame(w1, w2, w3)

#build the xbox data(Box size in real space)
box_len <- 0.1
plot(t,Y, col=rgb(0,0,1,0.25), typ='l')
abline(v=5, col='black')
abline(v=5+box_len, col='black')

#build the color vector(use only when plt=T)
col_vec <- c('red','blue', 'green')

##run the frequency mapping
L <- freq_map(t,Y, w_int = w_dat, xbox = box_len)
