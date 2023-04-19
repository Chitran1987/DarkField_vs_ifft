rm(list=ls())
library(readr)
library(tuneR)
#path of file
file_audio_path <- "D:/SynologyDrive/Didos_self/Career/Future/blog/LEEM_vs_inv_FFT/DarkField_vs_ifft/my_voice_01.wav"
#Read Files
my_audio = readWave(file_audio_path)
#Lets see the structure of the audio.
str(my_audio)  ## str diplays the structure of an arbitrary R object
#check the headers of the object
head(my_audio)
left <- my_audio@left
right <- my_audio@right
t <- seq(0, (length(left)-1))*(1/(my_audio@samp.rate))
plot(t, left, type='l', col=rgb(0,0,1,0.25))
lines(t, left, type='l', col=rgb(0,1,0,0.25))

#convert to a dataframe
my_voice_01 <- data.frame(t, left, right)
names(my_voice_01) <- c('time', 'left', 'right')
save(my_voice_01, file = 'my_voice_01.rda')
