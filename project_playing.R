library(plotly)
library(dplyr)
load('/Users/laramaleyeff/Downloads/loan.Rdata')
set.seed(100)
d <- loan.dat[sample(nrow(loan.dat), 1000), ]
attach(d)
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

