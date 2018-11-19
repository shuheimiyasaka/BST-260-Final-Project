# BST 260
# Final Project
# Purpose:
#   R script to simulate training and test 
#   data for our supervised machine learning algo
# Last Modified by CB 11/19/2018

rm(list=ls())
setwd('/Users/shuheimiyasaka/Google Drive/Harvard/Courses/BST 260/BST-260-Final-Project/')
library(truncnorm)
set.seed(1)

# create a function to produce errors which are correlated with the x-variable
create_corr_errors <- function(var1,var2,r){
  
  #var1 is the first variable, which remains unchanged (X in our case)
  #var2 is the second variable, which will be transformed (e in our case)
  #r is the correlation, 0 <= r <= 1
  
  indep.mat <- cbind(var1,var2) %*% solve(chol(var(cbind(var1,var2))))
  my.corr <- matrix(c(1,r,r,1),nrow=2,ncol=2)
  correlated.data <- indep.mat %*% chol(my.corr) * sd(var1)
  
  return(correlated.data[,2]) #returns the 2nd column of the matrix, which is the transformed var2
}

# create a function to simulate data
generate_Simulated_Data <- function(n, dist.errors, corr_err,
                                    data.generating.mechanism, rep_num)
{
  # generate the x's
  x <-rtruncnorm(n, a=0, b=Inf, mean=5, sd=2)
  
  # generate the errors
  if (corr_err == 'Independent'){
    
    if (dist.errors == 'Normal(0,1)'){
      err <- rnorm(n, 0, 1)
    } else if (dist.errors == 'Normal(0,2)'){
      err <- rnorm(n, 0, 2)
    } else if (dist.errors == 'Log normal(0,1)'){
      err <- rlnorm(n, 0, 1)
    } else if (dist.errors == 'Cauchy(loc=0, scale=1)'){
      err <- rcauchy(n, 0, 1)
    } else if (dist.errors == 'Student-t(df=2)'){
      err <- rt(n, 2)
    } else if (dist.errors == 'Gamma(5,1)'){
      err <- rgamma(n, 5, 1)
    } else if (dist.errors == 'Chi-squared(df=3)'){
      err <- rchisq(n, 3)
    } else if (dist.errors == 'Beta(5,1)'){
      err <- rbeta(n, 5, 1)
    } else if (dist.errors == 'Weibull(1,1)'){
      err <- rweibull(n, 1, 1)
    } else if (dist.errors == 'Exponential(1)'){
      err <- rexp(n, 1)
    } else stop(paste('Unknown Error Distribution: ',
                      dist.errors, sep =''))
    
  } else if (corr_err == '0.8 Corr w/ X'){
    
    if (dist.errors == 'Normal(0,1)'){
      pre_err <- rnorm(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Normal(0,2)'){
      pre_err <- rnorm(n, 0, 2)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Log normal(0,1)'){
      pre_err <- rlnorm(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Cauchy(loc=0, scale=1)'){
      pre_err <- rcauchy(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Student-t(df=2)'){
      pre_err <- rt(n, 2)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Gamma(5,1)'){
      pre_err <- rgamma(n, 5, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Chi-squared(df=3)'){
      pre_err <- rchisq(n, 3)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Beta(5,1)'){
      pre_err <- rbeta(n, 5, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Weibull(1,1)'){
      pre_err <- rweibull(n, 1, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else if (dist.errors == 'Exponential(1)'){
      pre_err <- rexp(n, 1)
      err <- create_corr_errors(x,pre_err,0.8)
    } else stop(paste('Unknown Error Distribution: ',
                      dist.errors, sep =''))
  
  } else if (corr_err == '0.2 Corr w/ X'){
    
    if (dist.errors == 'Normal(0,1)'){
      pre_err <- rnorm(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Normal(0,2)'){
      pre_err <- rnorm(n, 0, 2)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Log normal(0,1)'){
      pre_err <- rlnorm(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Cauchy(loc=0, scale=1)'){
      pre_err <- rcauchy(n, 0, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Student-t(df=2)'){
      pre_err <- rt(n, 2)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Gamma(5,1)'){
      pre_err <- rgamma(n, 5, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Chi-squared(df=3)'){
      pre_err <- rchisq(n, 3)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Beta(5,1)'){
      pre_err <- rbeta(n, 5, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Weibull(1,1)'){
      pre_err <- rweibull(n, 1, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else if (dist.errors == 'Exponential(1)'){
      pre_err <- rexp(n, 1)
      err <- create_corr_errors(x,pre_err,0.2)
    } else stop(paste('Unknown Error Distribution: ',
                      dist.errors, sep =''))
  }
  
  # generate the y's
  if (data.generating.mechanism == 'x'){
    y <- x + err
  } else if (data.generating.mechanism == 'x + x^2'){
    y <- x + x^2 + err  
  } else if (data.generating.mechanism == 'x + x^2 + x^3'){
    y <- x + x^2 + x^3 + err  
  } else if (data.generating.mechanism == 'sqrt(x)'){
    y <- sqrt(x) + err 
  } else if (data.generating.mechanism == 'log(x)'){
    y <- log(x) + err 
  } else if (data.generating.mechanism == 'exp(x)'){
    y <- exp(x) + err 
  } else stop(paste('Unknown Data Generating Mechanism: ',
                    data.generating.mechanism, sep = ''))
  
  sim.data.lab <- paste('n=', toString(n), '; ',
                        'dist error: ', dist.errors, '; ',
                        'data gen. mech.: ', data.generating.mechanism, ' ',
                        'rep: ', toString(r),
                        sep='')
  sim.data <- data.frame(x=x, y=y, n = rep(n, length(x)),
                         dist_errors = rep(dist.errors, length(x)),
                         Homoscedasticity = rep(corr_err, length(x)),
                         data_gen_mech = rep(data.generating.mechanism, length(x)),
                         sim_type = rep(sim.data.lab, length(x)),
                         rep_num = r)
  return(sim.data)
}

######################################################
### define some parameters to simulate data###########
######################################################

# sample size
num_replicates = 10 # for testing; do 1000 for real thing
n <- c(20, 100, 1000)

# distribution of the errors
dist.errors <- c('Normal(0,1)', 
                 'Normal(0,2)',
                 'Cauchy(loc=0, scale=1)',
                 'Student-t(df=2)',
                 'Log normal(0,1)',
                 'Gamma(5,1)',
                 'Chi-squared(df=3)',
                 'Beta(5,1)',
                 'Weibull(1,1)',
                 'Exponential(1)')

Homoscedasticity <- c('Independent',
                      '0.8 Corr w/ X',
                      '0.2 Corr w/ X')
# for testing
# Homoscedasticity <- c('Independent')

# how to generate the y
data.generating.mechanism <- c('x',
                               'x + x^2',
                               'x + x^2 + x^3',
                               'sqrt(x)',
                               'log(x)',
                               'exp(x)')
######################################################
### end defining some parameters to simulate data#####
######################################################

# simulate the data
sim.data <- NULL
for (sample_size in n){
  
  for (err in dist.errors){
    
    for (corr_err in Homoscedasticity){
    
      for (dat.gen.mech in data.generating.mechanism){
        
        for (r in 1:num_replicates){
        
          sim.data.temp <- generate_Simulated_Data(sample_size, err, corr_err,
                                                   dat.gen.mech, r)
          sim.data <- rbind(sim.data, sim.data.temp)
          
        } # end for (r in 1:num_replicates){
        
      } # end for (dat.gen.mech in data.generating.mechanism){
    
    } # for (corr_err in Homoscedasticity){
    
  } # end for (err in dist.errors){
  
} # end for (sample_size in n){

write.csv(sim.data, './simulated_data.csv')
save(sim.data, file = "./simulated_data.RData")
