# R Programming - April 2014
# Programming Assignment 1 Part 3

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)


# Write a function that takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and nitrate for
# monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0. A prototype of this function follows 

# corr <-
# function(directory, threshold = 0) { ## 'directory' is a character vector of
# length 1 indicating ## the location of the CSV files
# 
# ## 'threshold' is a numeric vector of length 1 indicating the ## number of
# completely observed observations (on all ## variables) required to compute the
# correlation between ## nitrate and sulfate; the default is 0
# 
# ## Return a numeric vector of correlations 
# } 

# For this function you will need
# to use the 'cor' function in R which calculates the correlation between two
# vectors. Please read the help page for this function via '?cor' and make sure
# that you know how to use it. You can see some example output from this
# function. The function that you write should be able to match this output.
# Please save your code to a file named corr.R. To run the submit script for
# this part, make sure your working directory has the file corr.R in it.

# source("corr.R")
# source("complete.R")
# cr <- corr("specdata", 150)
# head(cr)
# ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
# summary(cr)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
# cr <- corr("specdata", 400)
# head(cr)
# ## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
# summary(cr)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
# cr <- corr("specdata", 5000)
# summary(cr)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## 
# length(cr)
# ## [1] 0
# cr <- corr("specdata")
# summary(cr)
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
# length(cr)
# ## [1] 323