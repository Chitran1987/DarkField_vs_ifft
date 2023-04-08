df <- ft(X,Y, w=T)
ClearPlot()
##subplots
#First_investigation_kspace_line
par(mfrow=c(2,2))
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'phase', sub = 'Phase spectrum')
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'phase', sub = 'Phase spectrum')

#First_investigation_kspace_bubble
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'phase', sub = 'Phase spectrum')
plot(df$wf, abs(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'phase', sub = 'Phase spectrum')

##checking position of exact peak
par(mfrow=c(1,1))
ClearPlot()
plot(df$wf, abs(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(20.8,21.2), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
abline(v=20.9, col='red')
abline(v=21.0, col='red')

##Divide real spectrum into bins
n <- 4 # minimum no. of waves of this periodicity in the bin
p <- k/0.01 #no. of points available in a single unit of periodicity
N <- ceiling(n*p) #no. of data points needed for 4units of periodicity
