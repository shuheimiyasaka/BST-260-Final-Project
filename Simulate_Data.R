# BST 260
# Final Project
# Purpose:
#   R script to simulate training and test 
#   data for our supervised machine learning algo
# Last Modified by SM 10/29/2018

rm(list=ls())
set.seed(1)

# create a function to simulate data
generate_Simulated_Data <- function(n, dist.errors, data.generating.mechanism)
{
  # generate the x's
  x <-rnorm(n, 0, 2)
  
  # generate the errors
  if (dist.errors == 'Normal(0,1)'){
    err <- rnorm(n, 0, 1)
  } else if (dist.errors == 'Normal(0,2)'){
    err <- rnorm(n, 0, 2)
  } else stop('Unknown Error Distribution!')
  
  # generate the y's
  if (data.generating.mechanism == 'x'){
    y <- x + err
  } else if (data.generating.mechanism == 'x^2'){
    y <- x^2 + err  
  } else if (data.generating.mechanism == 'x^3'){
    y <- x^3 + err  
  } else stop('Unknown Data Generating Mechanism!')
  
  sim.data.lab <- paste('n=', toString(n), '; ',
                        'dist error: ', dist.errors, '; ',
                        'data gen. mech.: ', data.generating.mechanism,
                        sep='')
  sim.data <- data.frame(x=x, y=y, n = rep(n, length(x)),
                         dist_errors = rep('Normal(0,1)', length(x)),
                         data_gen_mech = rep(data.generating.mechanism, length(x)),
                         sim_type = rep(sim.data.lab, length(x)))
  return(sim.data)
}

######################################################
### define some parameters to simulate data###########
######################################################

# sample size
n <- c(10, 25, 75, 250, 1000)

# distribution of the errors
dist.errors <- c('Normal(0,1)', 
                 'Normal(0,2)',
                 'Log normal',
                 'Gamma',
                 'Chi-squared',
                 'Beta',
                 'Weibull',
                 'Exponential')
# this is a shorter version for testing
dist.errors <- c('Normal(0,1)')

# how to generate the y
data.generating.mechanism <- c('x',
                               'x^2',
                               'x^3',
                               'sqrt(x)',
                               'log(x)',
                               'e^x')
data.generating.mechanism <- c('x', 'x^2', 'x^3')

######################################################
### end defining some parameters to simulate data#####
######################################################

# simulate the data
sim.data <- NULL
for (sample_size in n){
  
  for (err in dist.errors){
    
    for (dat.gen.mech in data.generating.mechanism){
      
      sim.data.temp <- generate_Simulated_Data(sample_size, err, 
                                          dat.gen.mech)
      sim.data <- rbind(sim.data, sim.data.temp)
    } # for (dat.gen.mech in data.generating.mechanism){
    
  } # end for (err in dist.errors){
  
} # end for (sample_size in n){

write.csv(sim.data, './simulated_data.csv')
save.image('./simulated_data.RData')
