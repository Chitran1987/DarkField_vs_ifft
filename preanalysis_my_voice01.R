library(StatsChitran)
spectra <- ft(my_voice_01$time, my_voice_01$left, w=T)
plot(spectra$wf, abs(spectra$fy), type='l', xlim=c(0,60000))
