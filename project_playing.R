library(dplyr)
load('/Users/laramaleyeff/Downloads/loan.Rdata')
library(tidyverse)
library(funModeling)
library(caret)
library(VIM)
library(mice)
library(ggcorrplot)
library(plotly)
library(pROC)
library(lubridate)
library(glmnet)
library(plotly)

meta_loans <- funModeling::df_status(loan.dat, print_results = FALSE)

cols.2.remove <- c('id', 'member_id', 'url', 'desc')
missing.data.col <- meta_loans$variable[meta_loans$p_na > 10.]
cols.2.remove <- c(cols.2.remove, missing.data.col)
cols.2.keep <- !(colnames(loan.dat) %in% cols.2.remove)
loan.dat <- loan.dat[, cols.2.keep]

query = loan.dat$annual_inc == 0.
query.na = is.na(query)

if (sum(query.na) > 0){
  query[query.na] = TRUE
}
if (sum(query) > 0){
  loan.dat = loan.dat[!query,]
} else stop('unexpected case')

query <- loan.dat$loan_status != 'Issued'
loan.dat <- loan.dat[query, ]

loan.dat$loan_status_bin <- "Bad"
query = loan.dat$loan_status == 'Fully Paid' | loan.dat$loan_status == 'Current' |
  loan.dat$loan_status == 'Does not meet the credit policy. Status:Fully Paid'
loan.dat$loan_status_bin[query] = 'Good'

loan.dat$loan_status_bin = as.factor(loan.dat$loan_status_bin)


loan.dat <- loan.dat %>%
  mutate(perc_funded_amnt_inv = funded_amnt_inv/funded_amnt,
         issue_d = as.character(issue_d),
         term = as.character(term)) %>%
  mutate(year = as.numeric(str_sub(issue_d, start = -4)))

query <- loan.dat$term == ' 36 months'
loan.dat$term[query] = 'Short'
loan.dat$term[!query] = 'Long'
loan.dat$term = as.factor(loan.dat$term)

query.na <- is.na(loan.dat$tot_coll_amt)
if (sum(query.na) >0){
  loan.dat$tot_coll_amt[query.na] = 0
}
loan.dat <- loan.dat %>%
  mutate(tot_coll_amt_gt0 = as.factor(tot_coll_amt > 0.))


predictors <- c('loan_amnt', 'funded_amnt',
                'int_rate', 'grade',
                'emp_length', 'home_ownership',
                'annual_inc', 'verification_status',
                'purpose',
                'addr_state', 'dti',
                'delinq_2yrs', 'inq_last_6mths',
                'open_acc', 'pub_rec', 
                'revol_bal', 'revol_util',
                'total_acc', 'initial_list_status',
                'application_type', 'acc_now_delinq',
                'tot_coll_amt_gt0', 'tot_cur_bal',
                'total_rev_hi_lim', 'perc_funded_amnt_inv',
                'term', 'year')

loan.dat <- loan.dat[,c(predictors,'loan_status_bin')]
model <- glm(factor(loan_status_bin)~loan_amnt, link=binomial, data=loan.dat)

mean(loan.dat$year, na.rm=TRUE)
min(loan.dat$year, na.rm=TRUE)
max(loan.dat$year, na.rm=TRUE)
unique(loan.dat$year, na.rm=TRUE)
set.seed(100)

library(plotly)
d <- loan.dat[sample(nrow(loan.dat), 1000), ]
d<-d[,c(3,4,7,14,24,9)]
data <-cor(d,d)
plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap", colorscale="Greys")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
df <- d %>% group_by(addr_state) %>% summarize(grade=getmode(grade),loan_amnt=mean(loan_amnt))


df$hover <- with(df, paste(addr_state, '<br>', "Grade", grade))
l <- list(color = toRGB("white"), width = 2)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p_states <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~loan_amnt, text = ~hover, locations = ~addr_state,
    color = ~loan_amnt, colors = 'Purples'
  ) %>%
  colorbar(title = "Loan amount ($)") %>%
  layout(
    title = 'State',
    geo = g
  )

df1 <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

plot_ly(d, x = annual_inc, y = loan_amnt, text = paste("Grade: ", grade),
        mode = "markers", color = loan_amnt, size = loan_amnt)%>%
  layout(
    title = "Annual income vs. loan amount",
    scene = list(
      xaxis = list(title = "Annual income"),
      yaxis = list(title = "Loan amount")
    ))

defaulted <- 
  c("Default", 
    "Charged Off",
    "Does not meet the credit policy. Status:Charged Off", 
    "In Grace Period", 
    "Late (16-30 days)", 
    "Late (31-120 days)")

loans <-
  loan.dat %>% filter(loan_status != 'Issued') %>% 
  mutate(default = ifelse(!(loan_status %in% defaulted), FALSE, TRUE))

na_to_zero_vars <-
  c("mths_since_last_delinq", "mths_since_last_record",
    "mths_since_last_major_derog")

loans <- 
  loans %>%
  mutate_at(.vars = na_to_zero_vars, .funs = funs(replace(., is.na(.), 0)))

convert_date <- function(x){
  as.Date(paste0("01-", x), format = "%d-%b-%Y")
}

chr_to_date_vars <- 
  c("issue_d", "last_pymnt_d", "last_credit_pull_d",
    "next_pymnt_d", "earliest_cr_line", "next_pymnt_d")

loans <-
  loans %>%
  mutate_at(.funs = funs(convert_date), .vars = chr_to_date_vars)

vars_to_remove <- 
  c("annual_inc_joint", "dti_joint", "policy_code", "id", "member_id",
    "emp_title", "url", "desc", "title", "open_acc_6m", "open_il_6m", 
    "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", 
    "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util",
    "total_rev_hi_lim", "inq_fi", "total_cu_tl", "inq_last_12m",
    "verification_status_joint", "next_pymnt_d", "sub_grade", "loan_status")

loans <- loans %>% select(-one_of(vars_to_remove))

set.seed(100)
d <- loans[sample(nrow(loans), 100000), ]

save(d, file='/Users/laramaleyeff/Dropbox/BST260/BST-260-Final-Project/project/d.Rdata')

