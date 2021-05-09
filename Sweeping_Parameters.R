# ------------------------------------------------------
# writing functions for equations and sweeping over parameters
# 05 May 2021
# Sarah K. Morris
# ------------------------------------------------------
#
library(ggplot2)

# S=cA^z describes species-area relationship

# ------------------------------------------------------
# FUNCTION species_area_curve
# description: creates a power function for S and A
# inputs: A is a vector of island areas
#         c is the intercept constant
#         z is the slope constant
# outputs: S is a vector of species richness
########################################################
species_area_curve <- function(A=1:5000,
                               c=0.5,
                               z=0.26) {
S <- c*(A^z)
  return(S)
  
} # end of species_area_curve
# ------------------------------------------------------
head(species_area_curve())

# ------------------------------------------------------
# FUNCTION species_area_plot
# description: plots species area curves with parameter values
# inputs: A = vector of areas
#         c = the intercept constant
#         z = the slope constant
# outputs: smoothed curve with parameters printed in graph
########################################################
species_area_plot <- function(A=1:5000,
                              c=0.5,
                              z=0.26) {
#ggplot doesn't play well with for loops
plot(x=A,y=species_area_curve(A,c,z),
     type="l", #line graph
     xlab="Island Area",
     ylab="S",
     ylim=c(0,2500))
  mtext(paste("c =",c,"z =",z),cex = 0.7)
  # mtext = margin text
  # cex = character expansion factor
  
} # end of species_area_plot
# ------------------------------------------------------

species_area_plot()

# build a grid of plots

# global variables
c_pars <- c(100, 150, 175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)
par(mfrow=c(3,4))
# specifying grid arrangement of plots with 3 rows and 4 columns

for (i in seq_along(c_pars)) {
  for (j in seq_along(z_pars)) {
    species_area_plot(c = c_pars[i], z = z_pars[j])
  }
}

# expand.grid---------------------------------------------
expand.grid(c_pars,z_pars)

# ------------------------------------------------------
# FUNCTION sa_output
# description: summary stats for species-area power function
# inputs: vector of predicted species richness values
# outputs: list of max-min, coeff. of variation
########################################################
sa_output <- function(S = runif(10)) {
  
  sum_stats <- list(s_gain=max(S)-min(S),
                    s_cv=sd(S)/mean(S))
  
  return(sum_stats)
  
} # end of sa_output
# ------------------------------------------------------
sa_output()

# build program body -----------------------------------

# Global variables
Area <- 1:5000
c_pars <- c(100, 150, 175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)

# set up model data frame
model_frame <- expand.grid(c=c_pars, z=z_pars)
str(model_frame)
model_frame$SGain <- NA
model_frame$SCV <- NA
head(model_frame)

# cycle through model calculations
for (i in 1:nrow(model_frame)) {
  
  # generate S vector
  temp1 <- species_area_curve(A=Area,
                              c=model_frame[i,1],
                              z=model_frame[i,2])
  # calculate output stats
  temp2 <- sa_output(temp1)

  # pass results to columns in data frame
  model_frame[i,c(3,4)] <- temp2
  
}
print(model_frame)

# how to sweep parameters - redux w/ ggplot graphics-----------------------

area <- 1:5
c_pars <- c(100, 150, 175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)

# set up model frame
model_frame <- expand.grid(c=c_pars,
                           z=z_pars,
                           A=area)
head(model_frame)
nrow(model_frame) #60 rows b/c 5*3*4 to get all possible combinations

# add response variable
model_frame$S <- NA

# loop through parameters and fill with sa function

for (i in 1:length(c_pars)){
  for (j in 1:length(z_pars)) {
    model_frame[model_frame$c==c_pars[i] & model_frame$z==z_pars[j],"S"] <- species_area_curve(A=area, c=c_pars[i], z=z_pars[j])
  }
}

# ggplot graphics

p1 <- ggplot(data=model_frame)
p1 + geom_line(mapping=aes(x=A,y=S))+
  facet_grid(c~z)

p2 <- p1
p2 + geom_line(mapping=aes(x=A,y=S,group=z)) + 
  facet_grid(.~c)

p3 <- p1
p3 + geom_line(mapping=aes(x=A,y=S,group=c)) +
  facet_grid(z~.)






















