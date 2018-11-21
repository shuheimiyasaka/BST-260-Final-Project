# BST 260
# Purpose:
#  Data exploration of the loans data set
# Last modified by SM

rm(list = ls())
library(tidyverse)
library(funModeling)
library(caret)

setwd('/Users/shuheimiyasaka/Google Drive/BST 260 Final Project/')
load('loan.Rdata')
#loan.dat <- read.csv('loan.csv', header = TRUE)
#save(loan.dat, file = "./loan.RData")

#extract.2000 <- sample(1:dim(loan.dat)[1], 2000, replace = FALSE)
#write.csv(loan.dat[extract.2000, ], './loan_subset2000.csv')

meta_loans <- funModeling::df_status(loan.dat, print_results = FALSE)

meta_loans
