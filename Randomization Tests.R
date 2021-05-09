# ------------------------------------------------------
# Randomization Tests
# 05 May 2021
# Sarah K. Morris
# ------------------------------------------------------
#
# statistical p is the probability of obtaining the observed
# results (or something more extreme) if the null hyp were true 
# p(data|H0)

# null hypothesis is hypothesis of 'no effect'
# variation is caused by measured error or other 
# unspecified (and less important) sources of variation

# two advantages of randomization tests
# relaxes assumptions of standard parametric tests 
# (normality, balanced sample sizes, common variance)
# & gives more intuitive understanding of statistical probability

# Steps in a randomization test---------------------------

# 1. Define a metric X as a single number 
# to represent pattern that we are interested in
# i.e. F stat in an ANOVA (or sum of squares metric)
# has to be a single number

# 2. Calculate X(obs)
# the metric for the empirical (=observed) data that we start with

# 3. randomize or reshuffle the data. Randomize in a way that would uncouple the association b/n observed data and their assignment to treatment groups. Ideally the randomization only effects the pattern of treatment effects in the data. Other aspects of the data (i.e. sample sizes) are preserved in the randomization. Simulate the null hypothesis

# 4. For this single randomization, calculate X(sim)
# if the null hyp is true, then X(sim) should be similar to X(obs)

# 5. Repeat steps (3) & (4) many times (typically n=1000). 
# This will let us visualize as a histogram the distribution of X(sim); distribution of X values when the null hypothesis is true

# 6. Estimate the tail probability of the observed metric (or something more extreme) given the null distribution (p(X(obs)|H0).

# Preliminaries -----------------------------------

library(ggplot2)
library(TeachingDemos)

# random number generator we've been using is pseudorandom
# algorithm that generates a sequence of numbers that appear as if they were random

set.seed(100) # will allow for same values - sets initial random values

# char2seed takes any character string and converts it to a seed
char2seed("espresso withdrawal")   

char2seed("espresso withdrawal", set=FALSE)
  
Sys.time()
as.numeric(Sys.time())  # time in seconds or milliseconds maybe
my_seed <- as.numeric(Sys.time())
set.seed(my_seed)  
  
char2seed("espresso withdrawal") 

# create treatment groups
trt_group <- c(rep("Control",4),rep("Treatment",5))
# always a good idea to keep sample sizes NOT symmetrical   
  
# create response variable
z <- c(runif(4) + 1, runif(5) + 10)

# combine vectors into a dataframe
df <- data.frame(trt=trt_group, res=z) 
print(df)

# look at means in the 2 groups
obs <- tapply(df$res, df$trt, mean)
# tapply takes 1) thing you want to look at, 2) groups, 3) command to run
print(obs)

# time to reshuffle the dataset
# create a simulated dataset
# set up a new dataframe
df_sim <- df
# sample function changes ordering position of values
df_sim$res <- sample(df_sim$res)
print(df_sim)
 
# look at mean in 2 groups of randomized data
sim <- tapply(df_sim$res, df_sim$trt, mean)
print(sim)

# build functions -----------------------------------

# ------------------------------------------------------
# FUNCTION read_data
# description: read in (or generate) data set for analysis  
# inputs: file name (or nothing)
# outputs: 3 column dataframe of observed data (ID,x,y)
########################################################
read_data <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    df <- data.frame(ID=seq_along(x_obs),x_obs, y_obs)}
  
  #df <- read.table(file=z, header=TRUE, stringsAsFactors = FALSE)
  return(df)
  
} # end of read_data
# ------------------------------------------------------
# read_data() # test function 

# ------------------------------------------------------
# FUNCTION get_metric
# description: calculate metric for randomization test
# inputs: 2 column dataframe for regression
# outputs: regression slope
########################################################
get_metric <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    z <- data.frame(ID=seq_along(x_obs),x_obs, y_obs)}
  
  . <- lm(z[,3]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  
  slope <- .
  return(slope)
  
} # end of get_metric
# ------------------------------------------------------
# get_metric() # test function

# ------------------------------------------------------
# FUNCTION shuffle_data
# description: randomize data for regression analysis
# inputs: 3-column data frame (ID,xVar,yVar)
# outputs: 3-column data frame (ID,xVar,yVar)
########################################################
shuffle_data <- function(z=NULL) {
  if(is.null(z)){
    x_obs <- 1:20
    y_obs <- x_obs + 10*rnorm(20)
    z <- data.frame(ID=seq_along(x_obs),x_obs, y_obs)}
  
  z[,3] <- sample(z[,3]) # reshuffle column
  
  return(z)
  
} # end of shuffle_data
# ------------------------------------------------------
# shuffle_data() # test function

# ------------------------------------------------------
# FUNCTION get_pval
# description: calculate p value from simulation
# inputs: list of observed metric, and vector of simulated metrics
# outputs: lower, upper tail probability values
########################################################
get_pval <- function(z=NULL) {
  if(is.null(z)){
    z <- list(rnorm(1),rnorm(1000)) }
  p_lower <- mean(z[[2]]<=z[[1]]) 
  # what proportion of 1000 cases in which simulated value is less than the observed value (lower tail probability)
  p_upper <- mean(z[[2]]>=z[[1]]) # double bracket pulls out item from list
  # what proportion of 1000 simulated slopes are greater than the observed slope 
  # z[1] = observed slope & z[2] = 1000 simulated slopes
  return(c(pL=p_lower,pU=p_upper)) # these have to sum to 1
  # we have values above and below; area under curve = 1
} # end of get_pval
# ------------------------------------------------------
#get_pval() # test function 

# ------------------------------------------------------
# FUNCTION plot_ran_test
# description: create ggplot of histogram of simulated values
# inputs: list of observed metric and vector of simulated metrics
# outputs: saved ggplot graph
########################################################
plot_ran_test <- function(z=NULL) {
  if(is.null(z)){
    z <- list (rnorm(1), rnorm(1000))}
  
  df <- data.frame(ID=seq_along(z[[2]]),sim_x=z[[2]])
  p1 <- ggplot(data=df, mapping=aes(x=sim_x))
  p1 + geom_histogram(mapping=aes(fill=I("goldenrod"), color=I("black")))+
    geom_vline(aes(xintercept=z[[1]],col="blue")) # vertical line for obs slope
  # tail probabilities are the left and right of the line
  # histogram is the distribution of 1000 simulated slopes
  
} # end of plot_ran_test
# ------------------------------------------------------
# plot_ran_test() # test function

########################################################################
#### PUTTING IT ALL TOGETHER ###########################################
########################################################################

n_sim <- 1000 # number of simulated data sets
x_sim <- rep(NA, n_sim) # set up empty vector for simulated slopes
df <- read_data() # get (fake) data
x_obs <- get_metric(df) # get slope of observed data

# seq_len looks for a single number to create sequence
for (i in seq_len(n_sim)) {
  x_sim[i] <- get_metric(shuffle_data(df)) # run simulation
}

slopes <- list(x_obs,x_sim) 
get_pval(slopes) 
# pL = .961
# pU = 0.039
# observed slope is larger than most of the simulated
plot_ran_test(slopes)
# blue line splits distribution into upper and lower tail
# observed slope is unusually large compared to simulated distribution

# 2 key questions in randomization:
# 1. what is the metric?
# 2. what is the method for shuffling data?



  
  
  



