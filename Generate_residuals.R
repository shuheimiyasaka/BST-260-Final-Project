# Final Project
# BST 260
# Generate residuals and add labels
# Last modified by SM 11/19/2018

rm(list=ls())
setwd('/Users/shuheimiyasaka/Google Drive/BST 260 Final Project')
load('simulated_data.RData')

sim.data$error_lab_4_classification <- 'Not normal'
query <- sim.data$dist_errors == 'Normal(0,1)' |
  sim.data$dist_errors == 'Normal(0,2)'
if (sum(query)>0){
  sim.data$error_lab_4_classification[query] = 'Normal'
} else stop('unexpected case!')

sim.data$err_var_lab_4_classification <- 'Not constant variance'
query <- sim.data$Homoscedasticity == 'Independent'
if (sum(query)>0){
  sim.data$err_var_lab_4_classification[query] = 'Constant'
} else stop('unexpected case!')

sim.data$linearity_lab_4_classification <- 'Non-linear'
query <- sim.data$data_gen_mech == 'x'
if (sum(query)>0){
  sim.data$linearity_lab_4_classification[query] = 'Linear'
} else stop('unexpected case!')

sim.data$resid <- NA
for (sim in unique(sim.data$sim_type)){
  
  subset <- sim.data$sim_type == sim
  fit <- lm(y ~ x, data = sim.data[subset,])
  sim.data$resid[subset] = resid(fit)
  
}

write.csv(sim.data, './simulated_data_w_resid.csv')
save(sim.data, file = "./simulated_data_w_resid.RData")
