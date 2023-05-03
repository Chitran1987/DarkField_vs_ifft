#rm(list=ls())
library('StatsChitran') #Call library
ClearPlot() #Clear the plot
t <- seq(0,10, by=1/60000) #define the time axis
fcomp <- c(1000, 5000, 7734) #set which frequencies to build
Y1 <- 3*sin(2*pi*fcomp[1]*t)*gauss(t, amp =5, sig = 0.7, mu=1 ) #define signal Y1
Y2 <- 5*sin(2*pi*fcomp[2]*t)*gauss(t, amp = 4, sig = 1, mu=3) #define signal Y2
Y3 <- 4*sin(2*pi*fcomp[3]*t)*gauss(t, amp = 4.3, sig = 3, mu=7) #define signal Y3
plot(t,Y2, col=rgb(0,0,1,0.25), typ='l')
lines(t,Y1, col=rgb(1,0,0,0.25), typ='l')
#lines(t,Y2, col=rgb(0,0,1,0.25), typ='l')
lines(t,Y3, col=rgb(0,1,0,0.25), typ='l')

#plot with nbin
plot(L[[2]]$f_data_X, L[[2]]$f_data_Y, col=rgb(0,0,1,0.25), type = 'l', xlab = 'X', ylab = 'Y (arb. units)')
lines(L[[1]]$f_data_X, L[[1]]$f_data_Y, col=rgb(1,0,0,0.25))
lines(L[[3]]$f_data_X, L[[3]]$f_data_Y, col=rgb(0,1,0,0.25))

#plot with xbox
plot(L[[3]]$f_data_X, L[[3]]$f_data_Y, col=rgb(0,1,0,0.25), type = 'l', xlab = 'X', ylab = 'Y (arb. units)')
lines(L[[1]]$f_data_X, L[[1]]$f_data_Y, col=rgb(1,0,0,0.25))
lines(L[[2]]$f_data_X, L[[2]]$f_data_Y, col=rgb(0,0,1,0.25))
