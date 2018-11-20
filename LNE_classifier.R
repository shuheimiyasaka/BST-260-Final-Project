# Final Project
# BST 260
# LNE classifier:
#   Test for linearity, normality, and error assumption
#   of the fitting model results
# Last modified by SM 11/19/2018

rm(list=ls())

# define the libraries to load
library(olsrr)
library(gam)
library(nortest)
library(normtest)
library(splines2)
library(tidyverse)

##################################
### Define Training Functions ####
##################################

error_train <- function(res_dat, alpha=0.05)
{
  
  train.res <- res_dat[!duplicated(res_dat$sim_type), 
                       c('sim_type', 'err_var_lab_4_classification')]
  train.res <- rename(train.res, truth= err_var_lab_4_classification)
  
  # here loss = 1 means miss classified
  #      loss = 0 means correct classification
  train.res$class.test1 <- rep(NA, dim(train.res)[1])
  train.res$loss.test1 <- rep(1, dim(train.res)[1])
  
  train.res$class.test2 <- rep(NA, dim(train.res)[1])
  train.res$loss.test2 <- rep(1, dim(train.res)[1])
  
  train.res$class.test3 <- rep(NA, dim(train.res)[1])
  train.res$loss.test3 <- rep(1, dim(train.res)[1])
  
  train.res$class.test4 <- rep(NA, dim(train.res)[1])
  train.res$loss.test4 <- rep(1, dim(train.res)[1])
  
  counter = 1
  for (sim in unique(res_dat$sim_type)){
    
    subset <- res_dat$sim_type == sim
    
    # test 1
    test1.res <- ols_test_bartlett(res_dat[subset,], resid, y)
    if (test1.res$pval < alpha){
      train.res[counter, 'class.test1'] = 'Constant'
    } else train.res[counter, 'class.test1'] = 'Not constant variance'
    
    # test 2
    model <- lm(y ~ x, data = res_dat[subset,])
    test2.res <- ols_test_breusch_pagan(model)
    if (test2.res$p < alpha){
      train.res[counter, 'class.test2'] = 'Constant'
    } else train.res[counter, 'class.test2'] = 'Not constant variance'
    
    # test 3 
    model <- lm(y ~ x, data = res_dat[subset,])
    test3.res <- ols_test_score(model)
    if (test3.res$p < alpha){
      train.res[counter, 'class.test3'] = 'Constant'
    } else train.res[counter, 'class.test3'] = 'Not constant variance'
    
    # test 4
    model <- lm(y ~ x, data = res_dat[subset,])
    test4.res <- ols_test_f(model)
    if (test4.res$p < alpha){
      train.res[counter, 'class.test4'] = 'Constant'
    } else train.res[counter, 'class.test4'] = 'Not constant variance'
     
    counter = counter + 1
    
  } # end for (sim in unique(sim.data$sim_type)){
  
  # test 1
  query = train.res$class.test1 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test1'] = 0
  # test 2
  query = train.res$class.test2 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test2'] = 0  
  # test 3
  query = train.res$class.test3 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test3'] = 0  
  # test 4
  query = train.res$class.test4 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test4'] = 0  
  
  return(train.res)
  
} # end function error_train <- function(res_dat, alpha=0.05)

linearity_train <- function(res_dat, alpha=0.05)
{
  
  train.res <- res_dat[!duplicated(res_dat$sim_type), 
                       c('sim_type', 'linearity_lab_4_classification')]
  train.res <- rename(train.res, truth= linearity_lab_4_classification)
  
  # here loss = 1 means miss classified
  #      loss = 0 means correct classification
  train.res$class.test1 <- rep(NA, dim(train.res)[1])
  train.res$loss.test1 <- rep(1, dim(train.res)[1])
  
  train.res$class.test2 <- rep(NA, dim(train.res)[1])
  train.res$loss.test2 <- rep(1, dim(train.res)[1])
  
  counter = 1
  for (sim in unique(res_dat$sim_type)){
    
    subset <- res_dat$sim_type == sim
    
    # test 1
    test1.res <- gam(y~x+s(x,4), data = res_dat[subset,])
    test1.res <- summary(test1.res)$anova[3, 'Pr(F)']
    if (test1.res < alpha){
      train.res[counter, 'class.test1'] = 'Non-linear'
    } else train.res[counter, 'class.test1'] = 'Linear'
  
    # test 2
    test2.res <- lm(y ~ bSpline(x, knots=quantile(x, c(0.25, 0.5, 0.75)),
                                    degree=3), data=res_dat[subset,])
    lm.unadj <-lm(y ~ x, data=res_dat[subset,])
    test2.res <- anova(test2.res, lm.unadj)[2, "Pr(>F)"]
    if (test2.res < alpha){
      train.res[counter, 'class.test2'] = 'Non-linear'
    } else train.res[counter, 'class.test2'] = 'Linear'
      
    counter = counter + 1
    
  } # end for (sim in unique(res_dat$sim_type)){
  
  # test 1
  query = train.res$class.test1 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test1'] = 0
  # test 2
  query = train.res$class.test2 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test2'] = 0  
  
  return(train.res)
  
} # end function linearity_train <- function(res_dat, alpha=0.05)

normality_train <- function(res_dat, alpha=0.05)
{

  train.res <- res_dat[!duplicated(res_dat$sim_type), 
                       c('sim_type', 'error_lab_4_classification')]
  train.res <- rename(train.res, truth= error_lab_4_classification)
  
  # here loss = 1 means miss classified
  #      loss = 0 means correct classification
  train.res$class.test1 <- rep(NA, dim(train.res)[1])
  train.res$loss.test1 <- rep(1, dim(train.res)[1])
  
  train.res$class.test2 <- rep(NA, dim(train.res)[1])
  train.res$loss.test2 <- rep(1, dim(train.res)[1])
  
  train.res$class.test3 <- rep(NA, dim(train.res)[1])
  train.res$loss.test3 <- rep(1, dim(train.res)[1])
  
  train.res$class.test4 <- rep(NA, dim(train.res)[1])
  train.res$loss.test4 <- rep(1, dim(train.res)[1])
  
  counter = 1
  for (sim in unique(res_dat$sim_type)){
  
    subset <- res_dat$sim_type == sim
    
    # test 1
    test1.res <- lillie.test(res_dat$x[subset])$p.value
    if (test1.res < alpha){
      train.res[counter, 'class.test1'] = 'Not normal'
    } else train.res[counter, 'class.test1'] = 'Normal'
    
    # test 2
    test2.res <- shapiro.test(res_dat$x[subset])$p.value
    if (test2.res < alpha){
      train.res[counter, 'class.test2'] = 'Not normal'
    } else train.res[counter, 'class.test2'] = 'Normal'
    
    # test 3
    test3.res <- ajb.norm.test(res_dat$x[subset])$p.value
    if (test3.res < alpha){
      train.res[counter, 'class.test3'] = 'Not normal'
    } else train.res[counter, 'class.test3'] = 'Normal'
    
    # test 4
    test4.res <- ad.test(res_dat$x[subset])$p.value
    if (test4.res < alpha){
      train.res[counter, 'class.test4'] = 'Not normal'
    } else train.res[counter, 'class.test4'] = 'Normal'
    
    counter = counter + 1
    
  } # end for (sim in unique(res_dat$sim_type)){
  
  # test 1
  query = train.res$class.test1 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test1'] = 0
  # test 2
  query = train.res$class.test2 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test2'] = 0  
  # test 3
  query = train.res$class.test3 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test3'] = 0
  # test 4
  query = train.res$class.test4 == train.res$truth
  if (sum(query) > 0) train.res[query, 'loss.test4'] = 0  
  
  return(train.res)
  
} # end function normality_train <- function(res_dat, alpha=0.05)

#####################################
### End Training Function ###########
#####################################

####################################
### Define Testing Functions #######
####################################

error_predict <- function(test.dat, err.weights, alpha=0.05){

  # constant variance = 1
  # non-constant variance = 0

  model <- lm(y ~ x, data = test.dat)
  test.dat$resids <- residuals(model)
  
  # test 1
  test1.res <- ols_test_bartlett(test.dat, resids, y)
  if (test1.res$pval < alpha){
    test1.res = 1
  } else test1.res = 0
  
  # test 2
  test2.res <- ols_test_breusch_pagan(model)
  if (test2.res$p < alpha){
    test2.res = 1
  } else test2.res = 0
  
  # test 3 
  test3.res <- ols_test_score(model)
  if (test3.res$p < alpha){
    test3.res = 1
  } else test3.res = 0
  
  # test 4
  test4.res <- ols_test_f(model)
  if (test4.res$p < alpha){
    test4.res = 1
  } else test4.res = 0

  final.res = test1.res*err.weights[1] + test2.res*err.weights[2] + 
    test3.res*err.weights[3] + test4.res*err.weights[4]
  
  if (final.res>=0.5){
    return('Constant')
  } else return('Not constant variance')  
  
} # end error_predict 

linearity_predict <- function(test.dat, lin.weights, alpha=0.05){

  # linear = 1
  # non-linear = 0

  # test 1
  test1.res <- gam(y~x+s(x,4), data = test.dat)
  test1.res <- summary(test1.res)$anova[3, 'Pr(F)']
  if (test1.res < alpha){
    test1.res = 0
  } else test1.res = 1
  
  # test 2
  test2.res <- lm(y ~ bSpline(x, knots=quantile(x, c(0.25, 0.5, 0.75)),
                              degree=3), data=test.dat)
  lm.unadj <-lm(y ~ x, data=test.dat)
  test2.res <- anova(test2.res, lm.unadj)[2, "Pr(>F)"]
  if (test2.res < alpha){
    test1.res = 0
  } else test1.res = 1

  final.res = test1.res*lin.weights[1] + test2.res*lin.weights[2]
  
  if (final.res>=0.5){
    return('Linear')
  } else return('Non-linear')  
  
} # end linearity_predict <- function(test.dat, lin.weights, alpha=0.05){

normality_predict <- function(test.dat, norm.weights, alpha=0.05){
  
  # normal = 1
  # not normal = 0
  
  # test 1
  test1.res <- lillie.test(test.dat$x)$p.value
  if (test1.res < alpha){
    test1.res <- 0
  } else test1.res <- 1
  
  # test 2
  test2.res <- shapiro.test(test.dat$x)$p.value
  if (test2.res < alpha){
    test2.res <- 0
  } else test2.res <- 1
  
  # test 3
  test3.res <- ajb.norm.test(test.dat$x)$p.value
  if (test3.res < alpha){
    test3.res <- 0
  } else test3.res <- 1
  
  # test 4
  test4.res <- ad.test(test.dat$x)$p.value
  if (test4.res < alpha){
    test4.res <- 0
  } else test4.res <- 1

  final.res = test1.res*norm.weights[1] + test2.res*norm.weights[2] + 
    test3.res*norm.weights[3] + test4.res*norm.weights[4]
  
  if (final.res>=0.5){
    return('Normal')
  } else return('Not Normal')
  
}

####################################
### End Testing Functions ##########
####################################

##################################
### Run the Script Here ##########
##################################

setwd('/Users/shuheimiyasaka/Google Drive/Harvard/Courses/BST 260/BST-260-Final-Project/')
load('simulated_data_toydata_w_resid.RData')
sim.data$sim_type <- paste(sim.data$sim_type, ' homo: ', 
                           sim.data$Homoscedasticity, sep='')

query <- sim.data$rep_num <= ceiling(max(sim.data$rep_num)/2)
sim.data.train <- sim.data[query, ]
sim.data.test <- sim.data[!query, ]

### Start Training ###############

#err.res <- error_train(sim.data.train)
err.weights <- rep(1/4, 4)

#lin.res <- linearity_train(sim.data.train)
lin.weights <- rep(1/2, 2)

#norm.res <- normality_train(sim.data.train)

# for (i in 1:dim(norm.res)[1]){
# 
#   dat <- norm.res[i, c('loss.test1', 'loss.test2', 'loss.test3', 'loss.test4')]
#   fr <- function(x){
#     x1 <- x[1]
#     x2 <- x[2]
#     x3 <- x[3]
#     x4 <- x[4]
#     #(x1*(dat[,1]+.01) + x2*(dat[,2]+.01) + x3*(dat[,3]+.01) + x4*(dat[,4]+.01))
#     # minimize this function
#     (x1*(sum(dat[,1]^2)) + x2*(sum(dat[,2]^2)) + x3*(sum(dat[,3]^2)) + x4*(sum(dat[,4]^2)))
#   }
#   # rbind( c(-1,-1,-1,-1),
#   #        c(1,0,0,0), c(0,1,0,0),c(0,0,1,0), c(0,0,0,1)) %*% c(0.99,0.001,0.001,0.001)-c(-1,0,0,0,0)
#   weights.temp <- constrOptim(c(0.99,0.001,0.001,0.001), fr, NULL,
#                                 ui=rbind( c(-1,-1,-1,-1),
#                                           c(1,0,0,0), c(0,1,0,0),c(0,0,1,0), c(0,0,0,1) ),
#                                 ci=c(-1, 0.0001, 0.0001, 0.0001, 0.0001))$par
#   if (i == 1){
#     weights <- weights.temp
#   } else weights <- rbind(weights, weights.temp)
# 
# } # end for (i in 1:dim(dat)[1]){
# norm.weights <- colMeans(weights, na.rm = FALSE, dims = 1)
# norm.weights = norm.weights/sum(norm.weights)

###
# i think this is slightly better?
####

# dat <- norm.res[, c('loss.test1', 'loss.test2', 'loss.test3', 'loss.test4')]
# fr <- function(x){
#   x1 <- x[1]
#   x2 <- x[2]
#   x3 <- x[3]
#   x4 <- x[4]
#   #(x1*(dat[,1]+.01) + x2*(dat[,2]+.01) + x3*(dat[,3]+.01) + x4*(dat[,4]+.01))
#   # minimize this function
#   (x1*(sum(dat[,1]^2)) + x2*(sum(dat[,2]^2)) + x3*(sum(dat[,3]^2)) + x4*(sum(dat[,4]^2)))
# }
# # rbind( c(-1,-1,-1,-1),
# #        c(1,0,0,0), c(0,1,0,0),c(0,0,1,0), c(0,0,0,1)) %*% c(0.99,0.001,0.001,0.001)-c(-1,0,0,0,0)
# weights.temp <- constrOptim(c(0.99,0.001,0.001,0.001), fr, NULL,
#                             ui=rbind( c(-1,-1,-1,-1),
#                                       c(1,0,0,0), c(0,1,0,0),c(0,0,1,0), c(0,0,0,1) ),
#                             ci=c(-1, 0.0001, 0.0001, 0.0001, 0.0001))$par
# norm.weights = weights.temp/sum(weights.temp)
norm.weights <- rep(1/4, 4)

print('LNE Classifier Finish Training')
### End Training ###############

### Assess performance #########
alpha = 0.05

test.err.truth <- sim.data.test[!duplicated(sim.data.test$sim_type), 
                     c('err_var_lab_4_classification')]

test.lin.truth <- sim.data.test[!duplicated(sim.data.test$sim_type), 
                                 c('linearity_lab_4_classification')]

test.norm.truth <- sim.data.test[!duplicated(sim.data.test$sim_type), 
                                 c('error_lab_4_classification')]

counter = 1
for (sim in unique(sim.data.test$sim_type)){
  
  subset <- sim.data.test$sim_type == sim
  
  # constant variance tests
  err.ensemble.test.res.temp <- error_predict(sim.data.test[subset,], err.weights)
  
  test.dat.temp <- sim.data.test[subset,]
  model <- lm(y ~ x, data = test.dat.temp)
  test.dat.temp$resids <- residuals(model)
  
  test1.res.temp <- ols_test_bartlett(test.dat.temp, resids, y)
  if (test1.res.temp$pval < alpha){
    test1.res.temp = 'Constant'
  } else test1.res.temp = 'Not constant variance'

  test2.res.temp <- ols_test_breusch_pagan(model)
  if (test2.res.temp$p < alpha){
    test2.res.temp = 'Constant'
  } else test2.res.temp = 'Not constant variance'
  
  test3.res.temp <- ols_test_score(model)
  if (test3.res.temp$p < alpha){
    test3.res.temp = 'Constant'
  } else test3.res.temp = 'Not constant variance'
  
  test4.res.temp <- ols_test_f(model)
  if (test4.res.temp$p < alpha){
    test4.res.temp = 'Constant'
  } else test4.res.temp = 'Not constant variance'
    
  # linearity tests
  lin.ensemble.test.res.temp <- linearity_predict(sim.data.test[subset,], lin.weights)
  
  lin.gam.temp <- gam(y~x+s(x,4), data = sim.data.test[subset,])
  lin.gam.temp <- summary(lin.gam.temp)$anova[3, 'Pr(F)']
  if (lin.gam.temp < alpha){
    lin.gam.temp = 'Non-linear'
  } else lin.gam.temp = 'Linear'
  
  spline.fit <- lm(y ~ bSpline(x, knots=quantile(x, c(0.25, 0.5, 0.75)),
                              degree=3), data=sim.data.test[subset,])
  lm.unadj <-lm(y ~ x, data=sim.data.test[subset,])
  lin.spline.temp <- anova(spline.fit, lm.unadj)[2, "Pr(>F)"]
  if (lin.spline.temp < alpha){
    lin.spline.temp = 'Non-linear'
  } else lin.spline.temp = 'Linear'
  
  # normality tests
  norm.ensemble.test.res.temp <- normality_predict(sim.data.test[subset,], norm.weights)

  norm.lt.temp <- lillie.test(sim.data.test$x[subset])$p.value
  if (norm.lt.temp < alpha){
    norm.lt.temp <- 'Not Normal'
  } else norm.lt.temp <- 'Normal'
  
  norm.shap.temp <- shapiro.test(sim.data.test$x[subset])$p.value  
  if (norm.shap.temp < alpha){
    norm.shap.temp <- 'Not Normal'
  } else norm.shap.temp <- 'Normal'
  
  norm.ajb.temp <- ajb.norm.test(sim.data.test$x[subset])$p.value 
  if (norm.ajb.temp < alpha){
    norm.ajb.temp <- 'Not Normal'
  } else norm.ajb.temp <- 'Normal'
  
  norm.ad.temp <- ad.test(sim.data.test$x[subset])$p.value 
  if (norm.ad.temp < alpha){
    norm.ad.temp <- 'Not Normal'
  } else norm.ad.temp <- 'Normal'
  
  if (counter == 1){
    
    err.ensemble.test.res <- err.ensemble.test.res.temp
    err.test1.res <- test1.res.temp
    err.test2.res <- test2.res.temp
    err.test3.res <- test3.res.temp
    err.test4.res <- test4.res.temp
    
    lin.ensemble.test.res <- lin.ensemble.test.res.temp
    lin.gam.res <- lin.gam.temp
    lin.spline.res <- lin.spline.temp
    
    norm.ensemble.test.res <- norm.ensemble.test.res.temp
    norm.lt.res <- norm.lt.temp
    norm.shap.res <- norm.shap.temp
    norm.ajb.res <- norm.ajb.temp
    norm.ad.res <- norm.ad.temp
    
  } else {
    
    err.ensemble.test.res <- rbind(err.ensemble.test.res, err.ensemble.test.res.temp)
    err.test1.res <- rbind(err.test1.res, test1.res.temp)
    err.test2.res <- rbind(err.test2.res, test2.res.temp)
    err.test3.res <- rbind(err.test3.res, test3.res.temp)
    err.test4.res <- rbind(err.test4.res, test4.res.temp)
    
    lin.ensemble.test.res <- rbind(lin.ensemble.test.res, lin.ensemble.test.res.temp)
    lin.gam.res <- rbind(lin.gam.res, lin.gam.temp)
    lin.spline.res <- rbind(lin.spline.res, lin.spline.temp)
    
    norm.ensemble.test.res <- rbind(norm.ensemble.test.res, norm.ensemble.test.res.temp)
    norm.lt.res <- rbind(norm.lt.res, norm.lt.temp)
    norm.shap.res <- rbind(norm.shap.res, norm.shap.temp)
    norm.ajb.res <- rbind(norm.ajb.res, norm.ajb.temp)
    norm.ad.res <- rbind(norm.ad.res, norm.ad.temp)
    
  }
  
  counter = counter + 1
  
} # for (sim in unique(res_dat$sim_type)){

# constant err test performance
err.ensemble.test.res <- mean(err.ensemble.test.res == test.err.truth)
err.ensemble.test.res
err.test1.res <- mean(err.test1.res == test.err.truth)
err.test1.res
err.test2.res <- mean(err.test2.res == test.err.truth)
err.test2.res
err.test3.res <- mean(err.test3.res == test.err.truth)
err.test3.res
err.test4.res <- mean(err.test4.res == test.err.truth)
err.test4.res

# lineraity test performance
lin.ensemble.test.res <- mean(lin.ensemble.test.res == test.lin.truth)
lin.ensemble.test.res
lin.gam.res <- mean(lin.gam.res == test.lin.truth)
lin.gam.res
lin.spline.res <- mean(lin.spline.res == test.lin.truth)
lin.spline.res

# normal test performance
test.ensemble.norm.perm <- mean(norm.ensemble.test.res == test.norm.truth)
test.ensemble.norm.perm
test.lt.norm.perm <- mean(norm.lt.res == test.norm.truth)
test.lt.norm.perm
test.shap.norm.perm <- mean(norm.shap.res == test.norm.truth)
test.shap.norm.perm
test.ajb.norm.perm <- mean(norm.ajb.res == test.norm.truth)
test.ajb.norm.perm
test.ad.norm.perm <- mean(norm.ad.res == test.norm.truth)
test.ad.norm.perm

### End Assess performance #########

