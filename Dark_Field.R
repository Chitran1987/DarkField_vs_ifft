##Divide real spectrum into bins
n <- 4 # minimum no. of waves of this periodicity in the bin
p <- k/xsamp #no. of points available in a single unit of periodicity
N <- ceiling(n*p) #no. of data points needed for 4units of periodicity
