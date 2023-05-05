rm(list=ls())
library('StatsChitran') #Call library
ClearPlot() #Clear the plot
t <- seq(0,10, by=1/60000) #define the time axis
fcomp <- c(1000, 5000, 7734) #set which frequencies to build
Y1 <- 3*sin(2*pi*fcomp[1]*t)*gauss(t, amp =5, sig = 0.7, mu=1 ) #define signal w1 or Y1
Y2 <- 5*sin(2*pi*fcomp[2]*t)*gauss(t, amp = 4, sig = 1, mu=3) #define signal w2 or Y2
Y3 <- 4*sin(2*pi*fcomp[3]*t)*gauss(t, amp = 4.3, sig = 3, mu=7) #define signal w3 or Y3
Y <- Y1 + Y2 +Y3
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
df <- movavg(df$X, df$Y, bn=1000, fn=1000)
plot(df$X, df$Y, type = 'b', col=rgb(0,0,1,0.25))

#invoke library
library('NlcOptim')

#envelope function
env <- function(vec){
  res <- 0
  for (i in c(1,4,7)) {
    res <- res + gauss(df$X, amp = vec[i], mu = vec[i+1], sig = vec[i+2])
  }
  return(res)
}

#plot to optimize a good starting point
in_val <- c(3*10^6, 0.9, 0.7, 6*10^6, 3, 1, 5.5*10^6, 7, 3) #initial value
Y_fit <- env(in_val)
lines(df$X, Y_fit, col='red')


#objective function
obj_fun <- function(vec){
  err <- env(vec) - df$Y
  MSE <- mean(err^2)
  return(MSE)
}
#constraint
constr <- function(x){
  f <- NULL
  f <- rbind(f, 4*x[1] + 3*x[2] - 100)
  return(list(ceq = f, c = NULL))
}


#optimize and fit
vec_res <- solnl(in_val, objfun = obj_fun)
vec_res1 <- vec_res$par
draw_curv <- env(vec_res1)
lines(df$X, draw_curv, col='red')
#plot(df$X, draw)