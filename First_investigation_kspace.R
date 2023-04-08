df <- ft(X,Y, w=T)
ClearPlot()
##subplots
par(mfrow=c(2,2))
plot(df$wf, abs(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'l', col=rgb(0,0,1,0.25), xlab  = 'w', ylab = 'phase', sub = 'Phase spectrum')
plot(df$wf, abs(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'abs', sub = 'Magnitude spectrum')
plot(df$wf, Arg(df$fy), type= 'b', col=rgb(0,0,1,0.25), xlim = c(20.5,21.5), xlab = 'w', ylab = 'phase', sub = 'Phase spectrum')
