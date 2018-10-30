# BST 260
# Final Project
# Purpose:
#   R script to create a table of the simulation cases
# Last Modified by SM 10/29/2018

rm(list=ls())
library(stringr)
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

# how to generate the y
data.generating.mechanism <- c('x',
                               'x^2',
                               'x^3',
                               'sqrt(x)',
                               'log(x)',
                               'exp(x)')

######################################################
### end defining some parameters to simulate data#####
######################################################

# generate a table
counter = 1
for (sample_size in n){
  
  for (err in dist.errors){
    
    for (dat.gen.mech in data.generating.mechanism){
      
      sim.cases.temp <- data.frame(n = sample_size,
                                   dist_errors = err,
                                   data_gen_mech = dat.gen.mech,
                                   case = paste('Case ', str_pad(counter, 4, pad = "0"), sep=''))
      if (counter > 1){
        sim.cases <- rbind(sim.cases, sim.cases.temp)
      } else sim.cases <- sim.cases.temp
      
      counter = counter + 1
      
    } # for (dat.gen.mech in data.generating.mechanism){
    
  } # end for (err in dist.errors){
  
} # end for (sample_size in n){

write.csv(sim.cases, 'simulation_cases.csv')