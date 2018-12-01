library(shiny)
library(dplyr)


load('d.Rdata')
model <- glm(default ~addr_state+annual_inc+funded_amnt+loan_amnt+grade+funded_amnt_inv,family=binomial(link='logit'),data=d)

loans <- d

ui = fluidPage(
  # this is an input object   
  titlePanel("Loan Default Prediction Tool"),
  sidebarLayout(
              position = "left",
                sidebarPanel(
                  numericInput(inputId='annual_inc', label='Annual Income', value = round(mean(loans$annual_inc),-3),min = min(loans$annual_inc), max = max(loans$annual_inc), step = 100,width = NULL),
                  selectInput(inputId='grade', label='Grade', choices=unique(loans$grade)[order(unique(loans$grade))], selected = NULL,width = NULL),
                  selectInput(inputId='addr_state', label='State', choices=unique(loans$addr_state)[order(unique(loans$addr_state))], selected = NULL,width = NULL),
                  numericInput(inputId='funded_amnt', label='Funded Amount', value = round(mean(loans$funded_amnt),-3),min = min(loans$funded_amnt), max = max(loans$funded_amnt), step = 100,width = NULL),
                  numericInput(inputId='funded_amnt_inv', label='Funded Amount Investor', value = round(mean(loans$funded_amnt_inv),-3),min = min(loans$funded_amnt_inv), max = max(loans$funded_amnt_inv), step = 100,width = NULL),
                   numericInput(inputId='loan_amnt', label='Loan Amount', value = round(mean(loans$loan_amnt),-3),min = min(loans$loan_amnt), max = max(loans$loan_amnt), step = 100,width = NULL)
                  ),
                mainPanel(textOutput("Pred")))
)

server = function (input,output) {
  data <- reactive({
    req(input$addr_state, input$annual_inc, input$funded_amnt, input$funded_amnt_inv, input$loan_amnt, input$grade)
    data.frame(annual_inc=input$annual_inc,
               addr_state=input$addr_state,
               funded_amnt=input$funded_amnt,
               funded_amnt_inv=input$funded_amnt_inv,
               loan_amnt=input$loan_amnt,
               grade=input$grade
               )
  })
  
  pred <- reactive({
    p <- predict(model,data())
    round(100*exp(p)/(1+exp(p)),0)
  })
  
  output$Pred <- renderText({paste("The predicted percent chance of default is", pred(),"%.")})
}

shinyApp(ui=ui,server=server)