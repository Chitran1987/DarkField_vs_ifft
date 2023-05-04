#rm(list=ls())
library('StatsChitran') #Call library
ClearPlot() #Clear the plot
t <- seq(0,10, by=1/60000) #define the time axis
fcomp <- c(1000, 5000, 7734) #set which frequencies to build
Y1 <- 3*sin(2*pi*fcomp[1]*t)*gauss(t, amp =5, sig = 0.7, mu=1 ) #define signal w1 or Y1
Y2 <- 5*sin(2*pi*fcomp[2]*t)*gauss(t, amp = 4, sig = 1, mu=3) #define signal w2 or Y2
Y3 <- 4*sin(2*pi*fcomp[3]*t)*gauss(t, amp = 4.3, sig = 3, mu=7) #define signal w3 or Y3
plot(t,Y2, col=rgb(0,0,1,0.25), typ='l') #plot w2 or Y2
lines(t,Y1, col=rgb(1,0,0,0.25), typ='l') #plot w1 or Y1
#lines(t,Y2, col=rgb(0,0,1,0.25), typ='l')
lines(t,Y3, col=rgb(0,1,0,0.25), typ='l') #plot w3 or Y3
# Add a legend
legend("topright", legend = c("w1", "w2", "w3"), col = c("red", "blue", "green"), lty = 1, bg='transparent')

#plot with nbin
x_bin <- c(100, 100, 100)
L <- freq_map(t,Y, w_int = w_dat, nbin = x_bin, color = col_vec, plt = F)
plot(L[[2]]$f_data_X, L[[2]]$f_data_Y, col=rgb(0,0,1,0.25), type = 'l', xlab = 'X', ylab = 'Y (arb. units)')
lines(L[[1]]$f_data_X, L[[1]]$f_data_Y, col=rgb(1,0,0,0.25))
lines(L[[3]]$f_data_X, L[[3]]$f_data_Y, col=rgb(0,1,0,0.25))
legend("topleft", legend = c("w1", "w2", "w3"), col = c("red", "blue", "green"), lty = 1, bg='transparent')

#plot with xbox
box_len <- 0.1
L <- freq_map(t,Y, w_int = w_dat, xbox = box_len, color = col_vec, plt = F)
plot(L[[3]]$f_data_X, L[[3]]$f_data_Y, col=rgb(0,1,0,0.25), type = 'l', xlab = 'X', ylab = 'Y (arb. units)')
lines(L[[1]]$f_data_X, L[[1]]$f_data_Y, col=rgb(1,0,0,0.25))
lines(L[[2]]$f_data_X, L[[2]]$f_data_Y, col=rgb(0,0,1,0.25))
legend("topleft", legend = c("w1", "w2", "w3"), col = c("red", "blue", "green"), lty = 1, bg='transparent')

####fit a gaussian to profiles from nbin
#construct a dataframe and plot
df <- data.frame(L[[1]]$f_data_X, (L[[1]]$f_data_Y + L[[2]]$f_data_Y + L[[3]]$f_data_Y)) 
names(df) <- c('X','Y')
plot(df$X, df$Y, type = 'l', col=rgb(0,0,1,0.25))

#invoke library
library('NlcOptim')

#objective function
obj_fun <- function(vec){
  err <- gauss(df$X, amp = vec[1], mu=vec[2], sig = vec[3]) + gauss(df$X, amp = vec[4], mu=vec[5], sig = vec[6]) + gauss(df$X, amp = vec[7], mu=vec[8], sig = vec[9]) - df$Y
  MSE <- mean(err^2)
  return(MSE)
}

#initial value
in_val <- c(5*10^6,1,0.7,9*10^6,3,1,7*10^6,7,3)
vec_res <- solnl(in_val, objfun = obj_fun)
