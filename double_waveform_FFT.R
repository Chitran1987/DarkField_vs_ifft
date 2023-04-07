rm(list=ls()) ## remove list of loaded variables

##check if the graphics device is off(dev.cur() should return 1) and turn it off if open############
if(dev.cur() != 1){
  dev.off()
}
#################################################

x <- seq(-10, 10, by=0.01)
y <- vector(mode = 'numeric', length = length(x))
for (i in 1:length(x)) {
  if(x[i] <= -2){
    y[i]<- sin(3*x[i])
  }else{
    y[i]<- sin(7*x[i])
  }
}
plot(x,y)
fy <- fft(y)
fy_abs <- abs(fy)
fy_phase <- Arg(fy)
fr <- seq(0,1/0.01, by=1/(max(x)-min(x)))
plot(fr, fy_abs, xlim = c(0, 4), type = 'b')
plot(fr, fy_phase, xlim = c(0, max(fr)/2), type = 'b')
plot(x,y, xlim = c(-5,5))
y_new <- fft(fy, inverse = T)
plot(x,y_new, xlim = c(-5,5))   ###so the inverting works

##Now, check if the FFT can be filtered and inverted to give you the same
##implementing the filter
fy_filt <- vector(mode = 'numeric', length = length(fr))
for (i in 1:length(fr)) {
  if(fr[i]<=0.75){
    fy_filt[i] <-1 
  }else{
    fy_filt[i] <- 0
  }
}
#############################################################################

##multiplying the filter with the fourier transform and inverting##########################
y_new1 <- fft(fy*fy_filt, inverse = T)
plot(x, y_new1)
###########################################################################################

#so interestingly, the position of the wavefunctions are maintained

##check the new fourier spaces together
f_new <- fy*fy_filt
f_new_abs <- abs(f_new)
f_new_phase <- Arg(f_new)
plot(fr, fy_abs/max(fy_abs), xlim = c(0,4), col='blue', typ='b')
lines(fr, f_new_abs/max(f_new_abs), col='red', typ='b')
############################################################################################

plot(x, abs(y)/max(abs(y)), col='blue', typ='l')
lines(x, abs(y_new1)/max(abs(y_new1)), col='red', typ='l')
