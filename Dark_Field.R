##Need windowing desperately

##Divide real spectrum into bins
#rm(list=ls())
nbin <- 60 # minimum no. of waves of this periodicity in the bin
win_max <- 22 #The max value of the integration window in freq-space
win_min <- 20 #The min value of the integration window in freq space


xsamp <- mean(diff(X)) #calculate the sampling interval
p <- 2*pi/(win_min*xsamp) #no. of points available in a single unit of periodicity
N <- ceiling(nbin*p) #no. of data points needed for 2units of periodicity/for a single bin
n_dark_field_bins <- floor(length(X)/N) #no. of bins created by this dark field binning over entire data set






##start the dark field binning
bin_vec <- seq(1, n_dark_field_bins) #define a bin vector
dfld <- NULL #define a dark feild vector
d_orig <- data.frame(X,Y)
for (r in bin_vec) {
  d_tmp <- d_orig[((r-1)*N+1):(r*N),]  #subset your datatframe according to the bin
  dft_tmp_abs <- ft(d_tmp$X, d_tmp$Y, w=T)  #fourier transform it
  dft_tmp_abs$fy <- abs(dft_tmp_abs$fy) #convert the fourier coefficients to their magnitude
  int_val <- num_integrate(dft_tmp_abs$w, dft_tmp_abs$fy, xmin = win_min, xmax = win_max) #integrate between said (w values) k-vectors in k-space
  dfld <- c(dfld, int_val)
}
dfld <- data.frame(bin_vec, dfld) #convert it into a data frame

###recreating an envelope on the original dataset
f_data_X <- NULL
f_data_Y <- NULL
for (r in bin_vec) {
  f_data_X_tmp <- X[((r-1)*N+1):(r*N)]
  f_data_X <- c(f_data_X, f_data_X_tmp)
  f_data_Y_tmp <- rep(dfld$dfld[r], N)
  f_data_Y <- c(f_data_Y, f_data_Y_tmp)
}
f_data_Y <- nrm(f_data_Y)
f_data <- data.frame(f_data_X, f_data_Y)

##scaling f_data to look good for plotting
min_f <-  mean(Y)
max_f <- (max(Y) - mean(Y))/3 + mean(Y)    ##simple aesthetic judgement ;)
f_data$f_data_Y <- nrm(f_data$f_data_Y, min = min_f, max = max_f)

##plotting the data
ClearPlot()
plot(X,Y, col=rgb(0,0,1,0.25), type = 'l')
lines(f_data$f_data_X, f_data$f_data_Y, col='red')
