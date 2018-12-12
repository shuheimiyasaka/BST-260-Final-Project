library(shiny)
library(dplyr)
library(glmnet)
library(datasets)

load('final_model.Rdata')


ui = fluidPage(
  # this is an input object   
  titlePanel("Loan Default Prediction Tool"),
  sidebarLayout(
    
              position = "left",
                sidebarPanel(
                  numericInput(inputId='annual_inc', label='Annual Income', value = 75000,min = 4000, max = 6*10^6, step = 100,width = NULL),
                  selectInput(inputId='grade', label='Grade', choices=c('A'=1,'B'=2,'C'=3, 'D'=4,'E'=5, 'F'=6, 'G'=7), selected = 'A',width = NULL),
                  selectInput(inputId='addr_state', label='State', choices=state.abb, selected = 'AL',width = NULL),
                   numericInput(inputId='loan_amnt', label='Loan Amount', value =  15000,min =  1000, max =  35000, step = 100,width = NULL),
                  selectInput(inputId='term', label='Term', choices=c('Short', 'Long')),
                  numericInput(inputId='perc_funded_amnt_inv', label='Proportion Funded by Investors', value = 0, min = 0, max = 1, step = 0.01,width = NULL),
                  selectInput(inputId='emp_length', label='Employment length', choices=c('< 1 year'=0.5,'1 year'=1, '2 years'=2, '3 years'=3, '4 years'=4, '5 years'=5, '6 years'=6, '7 years'=7, '8 years'=8, '9 years'=9, '10+ years'=10)),
                  selectInput(inputId='home_ownership', label='Home ownership', choices=c('Any'='ANY','Mortgage'='MORTGAGE', 'Own'='OWN', 'Rent'='RENT', 'None'='NONE', 'Other'='OTHER')),
                  selectInput(inputId='verification_status', label='Verification status', choices=c('Source verified', 'Verified')),
                  selectInput(inputId='purpose', label='Purpose', choices=c('Credit card'= 'credit_card', 'Educational'='educational', 
                                                                             'House'='house', 'Medical'='medical', 'Moving'='moving', 
                                                                            'Renewable Energy'='renewable_energy', 'Small Business'='small_business', 'Vacation'='vacation', 
                                                                            'Wedding'='wedding',
                                                                            'Other'='other'), selected='credit_card'),
                  numericInput(inputId='dti', label='DTI', value = 18, min = 0, max = 9999, step = 10,width = NULL),
                  numericInput(inputId='delinq_2yrs', label='2 year delinquencies', value = 0, min = 0, max = 40, step = 1,width = NULL),
                  numericInput(inputId='inq_last_6mths', label='Inquiries in the last 6 months', value = 0, min = 0, max = 40, step = 1,width = NULL),
                  numericInput(inputId='open_acc', label='Open accounts', value = 0, min = 0, max = 90, step = 1,width = NULL),
                  numericInput(inputId='pub_rec', label='Number of derogatory public records', value = 0, min = 0, max = 90, step = 1,width = NULL),
                  numericInput(inputId='revol_bal', label='Total credit revolving balance', value = 20000, min = 0, max = 3000000, step = 1000,width = NULL),
                  numericInput(inputId='revol_util', label='Revolving line utilization rate', value = 55, min = 0, max = 900, step = 5,width = NULL),
                  numericInput(inputId='total_acc', label='The total number of credit lines currently in the borrower\'s credit file', value = 25, min = 0, max = 170, step = 1,width = NULL),
                  selectInput(inputId='initial_list_status', label='Initial list status', choices=c('F'='f', 'W'='w')),
                  selectInput(inputId='application_type', label='Application type', choices=c('Individual'='INDIVIDUAL', 'Joint'='JOINT'), selected='JOINT'),
                  checkboxInput(inputId='tot_coll_amt_gt0',label='Total collection amounts ever owed > 0', value=1),
                  numericInput(inputId='tot_cur_bal', label='Total current balance', value = 150000, min = 0, max = 10000000, step = 1000,width = NULL),
                  numericInput(inputId='total_rev_hi_lim', label='Total revolving high credit/credit limit', value = 150000, min = 0, max = 10000000, step = 1000,width = NULL),
                  style = "height: 90vh; overflow-y: auto;",
                  width = 4)
              
                   ,
                mainPanel(textOutput("Pred")))
)

server = function (input,output) {
  data <- reactive({
    req(input$annual_inc, input$grade, input$addr_state, input$loan_amnt,  input$term, input$perc_funded_amnt_inv, input$emp_length,
        input$home_ownership,input$verification_status, input$purpose,input$dti,input$delinq_2yrs,
        input$inq_last_6mths,input$open_acc,input$pub_rec, input$revol_bal,input$total_acc,
        input$initial_list_status,input$application_type,
        input$tot_cur_bal,input$total_rev_hi_lim)
    data.frame(
              intercept=1,
               loan_amnt=input$loan_amnt,
               funded_amnt=0,
               grade_ordinal=as.numeric(input$grade),
               emp_length_ordinal=as.numeric(input$emp_length),
               home_ownershipMORTGAGE=ifelse(input$home_ownership=='MORTGAGE',1,0),
               home_ownershipNONE=ifelse(input$home_ownership=='NONE',1,0),
               home_ownershipOTHER=ifelse(input$home_ownership=='OTHER',1,0),
               home_ownershipOWN=ifelse(input$home_ownership=='OWN',1,0),
               home_ownershipRENT=ifelse(input$home_ownership=='RENT',1,0),
               annual_inc=input$annual_inc,
               verification_statusSourceVerified=ifelse(input$verification_status=='Source Verified',1,0),
               verification_statusVerified=ifelse(input$verification_status=='Verified',1,0),
               purposecredit_card=ifelse(input$purpose=='credit_card',1,0),
               purposedebt_consolidation=ifelse(input$purpose=='debt_consolidation',1,0),
               purposeeducational=ifelse(input$purpose=='educational',1,0),
               purposehome_improvement=ifelse(input$purpose=='home_improvement',1,0),
               purposehouse=ifelse(input$purpose=='house',1,0),
               purposemajor_purchase=ifelse(input$purpose=='major_purchase',1,0),
               purposemedical=ifelse(input$purpose=='medical',1,0),
               purposemoving=ifelse(input$purpose=='moving',1,0),
               purposeother=ifelse(input$purpose=='other',1,0),
               purposerenewable_energy=ifelse(input$purpose=='renewable_energy',1,0),
               purposesmall_business=ifelse(input$purpose=='small_business',1,0),
               purposevacation=ifelse(input$purpose=='vacation',1,0),
               purposewedding=ifelse(input$purpose=='wedding',1,0),
               addr_stateAL=ifelse(input$addr_state=='AL',1,0),
               addr_stateAR=ifelse(input$addr_state=='AR',1,0),
               addr_stateAZ=ifelse(input$addr_state=='AZ',1,0),
               addr_stateCA=ifelse(input$addr_state=='CA',1,0),
               addr_stateCO=ifelse(input$addr_state=='CO',1,0),
               addr_stateCT=ifelse(input$addr_state=='CT',1,0),
               addr_stateDC=ifelse(input$addr_state=='DC',1,0),
               addr_stateDE=ifelse(input$addr_state=='DE',1,0),
               addr_stateFL=ifelse(input$addr_state=='FL',1,0),
               addr_stateGA=ifelse(input$addr_state=='GA',1,0),
               addr_stateHI=ifelse(input$addr_state=='HI',1,0),
               addr_stateIA=ifelse(input$addr_state=='IA',1,0),
               addr_stateID=ifelse(input$addr_state=='ID',1,0),
               addr_stateIL=ifelse(input$addr_state=='IL',1,0),
               addr_stateIN=ifelse(input$addr_state=='IN',1,0),
               addr_stateKS=ifelse(input$addr_state=='KS',1,0),
               addr_stateKY=ifelse(input$addr_state=='KY',1,0),
               addr_stateLA=ifelse(input$addr_state=='LA',1,0),
               addr_stateMA=ifelse(input$addr_state=='MA',1,0),
               addr_stateMD=ifelse(input$addr_state=='MD',1,0),
               addr_stateME=ifelse(input$addr_state=='ME',1,0),
               addr_stateMI=ifelse(input$addr_state=='MI',1,0),
               addr_stateMN=ifelse(input$addr_state=='MN',1,0),
               addr_stateMO=ifelse(input$addr_state=='MO',1,0),
               addr_stateMS=ifelse(input$addr_state=='MS',1,0),
               addr_stateMT=ifelse(input$addr_state=='MT',1,0),
               addr_stateNC=ifelse(input$addr_state=='NC',1,0),
               addr_stateND=ifelse(input$addr_state=='ND',1,0),
               addr_stateNE=ifelse(input$addr_state=='NE',1,0),
               addr_stateNH=ifelse(input$addr_state=='NH',1,0),
               addr_stateNJ=ifelse(input$addr_state=='NJ',1,0),
               addr_stateNM=ifelse(input$addr_state=='NM',1,0),
               addr_stateNV=ifelse(input$addr_state=='NV',1,0),
               addr_stateNY=ifelse(input$addr_state=='NY',1,0),
               addr_stateOH=ifelse(input$addr_state=='OH',1,0),
               addr_stateOK=ifelse(input$addr_state=='OK',1,0),
               addr_stateOR=ifelse(input$addr_state=='OR',1,0),
               addr_statePA=ifelse(input$addr_state=='PA',1,0),
               addr_stateRI=ifelse(input$addr_state=='RI',1,0),
               addr_stateSC=ifelse(input$addr_state=='SC',1,0),
               addr_stateSD=ifelse(input$addr_state=='SD',1,0),
               addr_stateTN=ifelse(input$addr_state=='TN',1,0),
               addr_stateTX=ifelse(input$addr_state=='TX',1,0),
               addr_stateUT=ifelse(input$addr_state=='UT',1,0),
               addr_stateVA=ifelse(input$addr_state=='VA',1,0),
               addr_stateVT=ifelse(input$addr_state=='VT',1,0),
               addr_stateWA=ifelse(input$addr_state=='WA',1,0),
               addr_stateWI=ifelse(input$addr_state=='WI',1,0),
               addr_stateWV=ifelse(input$addr_state=='WV',1,0),
               addr_stateWY=ifelse(input$addr_state=='WY',1,0),
               dti=input$dti,
               delinq_2yrs=input$delinq_2yrs,
               inq_last_6mths=input$inq_last_6mths,
               open_acc=input$open_acc,
               pub_rec=input$pub_rec,
               revol_bal=input$revol_bal,
               revol_util=input$revol_util,
               total_acc=input$total_acc,
               initial_list_statusw=ifelse(input$initial_list_status=='w',1,0),
               application_typeJOINT=ifelse(input$application_type=='JOINT',1,0),
               acc_now_delinq=1,
               tot_coll_amt_gt0TRUE=ifelse(input$tot_coll_amt_gt0,1,0),
               tot_cur_bal=input$tot_cur_bal,
               total_rev_hi_lim=input$total_rev_hi_lim,
               perc_funded_amnt_inv=input$perc_funded_amnt_inv,
               termShort=ifelse(input$term=='Short', 1, 0)
               )
  })
  
  pred <- reactive({
    p <- data.matrix(data()) %*%coef(final_mod)
    round(100*exp(1-p)/(1+exp(1-p)),4)
  })
  
  output$Pred <- renderText({paste("The predicted percent chance of default is", pred(),"%.")})
}

shinyApp(ui=ui,server=server)