library(shiny)
library(dplyr)
library(Quandl)

## Setup.
round_ <- function(x,l) round(x/l)*(l)

## Retrieve current spot rates.
zero_rates = Quandl("FED/SVENY", start_date="2017-03-10", end_date="2017-03-10")[-1]
names(zero_rates) = as.numeric(substr(names(zero_rates),6,7))
all_rates <- data.frame(tenor=names(zero_rates), zero=t(zero_rates), row.names=NULL, stringsAsFactors = F)
all_rates$tenor <- as.numeric(all_rates$tenor)

## Derived data.
all_rates$disc <- 1/((1+(all_rates$zero/100)) ** all_rates$tenor)
all_rates$fwd <- (all_rates$disc)/lead(all_rates$disc) - 1

## Fake data.
#tenors = seq(0.5, 30, 0.5)
#risk_free = c(seq(3,5, 0.1),seq(5.1,7,0.05))
#df <- data.frame(tenors, risk_free)

shinyServer(function(input, output) {
  v <- reactiveValues(df=all_rates,
                           click_x = NULL,
                           click_y = NULL)
  
  ## Bond properties.
  cash_flows <- reactive({
    c(rep(input$coupon, input$maturity*2), 100 + input$coupon)
  })
  ytm <- reactive({
    (100 / input$price)**(input$maturity*2) - 1 
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
    for(i in seq(-3, 3)) {
      prev = tmp_df#tmp_df[tmp_df['tenor'] == center+i, 'zero']
      print(tmp_df[tmp_df['tenor'] == center+i])
      tmp_df[tmp_df['tenor'] == center+i, 'zero'] = prev + dnorm(i, sd=2) * (input$plot_click$y - prev)
    }
    v$df <- tmp_df
    })
  
  ## Plot yield curve.
  output$risk_free <- renderPlot({
    with(v$df, plot(tenor, zero, ylim=c(0, 20), xlab='TTM', ylab='Yield (%)', pch=20))
    with(v$df, lines(tenor, zero, pch=16))
    title('Risk-Free Yield Curve')
  })
  
  ## Plot cash flows.
  output$cash_flows <- renderPlot({
    barplot(height=cash_flows(), names.arg=seq(0.5, input$maturity + 0.5, 0.5))
  })
  
  ## Output vars.
  output$ytm <- renderText({
    paste0('YTM=', ytm())
  })
})
