library(shiny)
library(dplyr)

## Setup.
round_ <- function(x,l) round(x/l)*(l)
linMap <- function(x, a, b) approxfun(range(0, max(x)), c(a, b))(x)
 
tenors = seq(0.5, 30, 0.5)
risk_free = c(seq(3,5, 0.1),seq(5.1,7,0.05))

df <- data.frame(tenors, risk_free)

shinyServer(function(input, output) {
  v <- reactiveValues(df=df,
                           click_x = NULL,
                           click_y = NULL)
  
  ## Bond properties.
  cash_flows <- reactive({
    c(rep(input$coupon, input$maturity*2), 100 + input$coupon)
  })
  
  discounted <- reactive({
    
  })
  
  ## Pick yield curve.
  observeEvent(input$plot_click, {
    v$click_x = round_(input$plot_click$x, 0.5)
    v$click_y = input$plot_click$y
    
    tmp_df <- v$df
    center = round_(input$plot_click$x, 0.5)
    ## Shocks.
    for(i in seq(-7, 7, 0.5)) {
      prev = tmp_df[tmp_df['tenors'] == center+i, 'risk_free']
      tmp_df[tmp_df['tenors'] == center+i, 'risk_free'] = prev + dnorm(i, sd=2) * (input$plot_click$y - prev)
    }
    v$df <- tmp_df
    })
  
  ## Plot yield curve.
  output$risk_free <- renderPlot({
    with(v$df, plot(tenors, risk_free, ylim=c(0, 20), xlab='TTM', ylab='Yield (%)', pch=20))
    with(v$df, lines(tenors, risk_free, pch=16))
    title('Risk-Free Yield Curve')
  })
  
  ## Plot cash flows.
  output$cash_flows <- renderPlot({
    barplot(height=cash_flows(), names.arg=seq(0.5, input$maturity + 0.5, 0.5))
  })
  
  output$info <- renderText({
    paste0('P=', input$price)
  })
})
