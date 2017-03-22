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
all_rates$fwd <- ((all_rates$disc)/lead(all_rates$disc) - 1)*100

## Fake data.
#tenors = seq(0.5, 30, 0.5)
#risk_free = c(seq(3,5, 0.1),seq(5.1,7,0.05))
#df <- data.frame(tenors, risk_free)

shinyServer(function(input, output) {
  v <- reactiveValues(df=all_rates,
                           click_x = NULL,
                           click_y = NULL,
                           maturity = 10,
                           coupon = 5)
  
  ## Bond properties.
  cash_flows <- reactive({
    c(rep(v$coupon, v$maturity-1), 100 + v$coupon)
  })
  disc_flows <- reactive({
    cash_flows() * v$df$disc[length(cash_flows())]
  })
  ## All flows.
  all_flows <- reactive({
    data.frame(cash_flows=cash_flows(), disc_flows=disc_flows())
  })
  
  ytm <- reactive({
    (100 / input$price)**(input$maturity*2) - 1 
  })
  
  ## Observe coupon.
  observeEvent(input$coupon, {
    v$coupon <- input$coupon
  })
  ## Observe maturity.
  observeEvent(input$maturity, {
    v$maturity <- input$maturity
  })  
  ## Observe yield curve.
  observeEvent(input$plot_click, {
    v$click_x <- round(input$plot_click$x)
    v$click_y <- input$plot_click$y
    
    tmp_df <- v$df
    center <- v$click_x
    ## Shocks.
    for(i in seq(-10, 10)) {
      prev <- tmp_df
      tmp_df[tmp_df['tenor'] == center+i, 'zero'] <- prev[prev['tenor'] == center+i, 'zero'] + 
        dnorm(i, sd=3) * (v$click_y - prev[prev['tenor'] == center+i, 'zero'])
    }
    tmp_df$disc <- 1/((1+(tmp_df$zero/100)) ** tmp_df$tenor)
    tmp_df$fwd <- ((tmp_df$disc)/lead(tmp_df$disc) - 1)*100
    v$df <- tmp_df
    })
  
  ## Update maturity.
  
  ## Plot yield curve.
  output$risk_free <- renderPlot({
    with(v$df, plot(tenor, zero, ylim=c(0, 10), xlab='TTM', ylab='Yield (%)', pch=20, col='#428bca'))
    with(v$df, lines(tenor, zero, pch=16, col='#428bca'))
    with(v$df, lines(tenor, fwd, pch=14, col='#999999'))
    title('USD Govt Zero Coupon')
  })
  
  ## Plot cash flows.
  output$cash_flows <- renderPlot({
    barplot(t(as.matrix(all_flows())), names.arg=seq(v$maturity), beside=T, cols=c('#0a0','#000000'))
  })
  
  ## Output vars.
  output$ytm <- renderText({
    paste0('YTM=', ytm())
  })
})
