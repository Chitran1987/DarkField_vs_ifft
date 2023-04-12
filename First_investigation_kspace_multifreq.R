df <- ft(X,Y, w=T)
ClearPlot()
plot(df$wf, abs(df$fy), xlim = c(2,25), ylim=c(0,0.4*10^4), type = 'l', col=rgb(0,0,1,0.25))
##limits
abline(v=3, col='red')
abline(v=7, col='red')
abline(v=7.25, col='blue')
abline(v=11, col='blue')
abline(v=19, col='green')
abline(v=23, col='green')




##subplots
#First_investigation_kspace_line
par(mfrow=c(2,2))
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=19, col='red')
abline(v=24, col='red')

# abline(h=5000, col='red')
# abline(h=0, col='red')

plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'phase', sub = 'Phase spectrum')
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlim = c(19,24), ylim = c(0,5000), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=20, col='red')
abline(v=22, col='red')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlim = c(19,24), ylim=c(0,3), xlab = 'w', ylab = 'phase', sub = 'Phase spectrum')

#First_investigation_kspace_bubble
ClearPlot()
par(mfrow=c(2,2))
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=19, col='red')
abline(v=24, col='red')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'phase', sub = 'Phase spectrum')
plot(df$wf, abs(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(19,24), ylim = c(0,5000), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=20, col='red')
abline(v=22, col='red')
plot(df$wf, Arg(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(19,24), ylim=c(0,3), xlab = 'w', ylab = 'phase', sub = 'Phase spectrum')

##checking position of exact peak
par(mfrow=c(1,1))
ClearPlot()
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlim = c(19,24), ylim=c(0,5000), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=20, col='red')
abline(v=22, col='red')

