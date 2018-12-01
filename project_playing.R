library(dplyr)
load('/Users/laramaleyeff/Downloads/loan.Rdata')
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

