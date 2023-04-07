rm(list=ls()) ## remove list of loaded variables

##check if the graphics device is off(dev.cur() shoul return 1) and turn it off if open############
if(dev.cur() != 1){
  dev.off()
}
#################################################


x <- seq(-100, 100, by=0.01)
y <- sin((2*pi)*x)
plot(x,y)
fy <- fft(y)
fy_abs <- abs(fy)
fy_arg <- Arg(fy)
fr <- seq(0, 1/0.01, by=1/(max(x)-min(x)))
#plot(fr, fy_abs, xlim = c(0, max(fr)/2))
plot(fr, fy_abs, xlim = c(0.5, 1.5), type = 'b')
#plot(fr, fy_arg, xlim = c(0, max(fr)/2), type='b')
plot(fr, fy_arg, xlim = c(0, 10), type='b')

###try FFT inverse
y_new <- fft(fy, inverse = 'T')
y_new_abs <- abs(y_new)
y_new_arg <- Arg(y_new)
plot(y, y_new_abs)
plot(y, y_new_arg, type = 'b')



plot(fr, fy_arg)
     