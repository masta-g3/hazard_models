library(shiny)
library(dplyr)
library(Quandl)

## Setup.
round_ <- function(x,l) round(x/l)*(l)

## Retrieve current spot rates.
#zero_rates = Quandl("FED/SVENY", start_date="2017-03-10", end_date="2017-03-10")[-1]
#names(zero_rates) = as.numeric(substr(names(zero_rates),6,7))
zero_rates <- read.csv('zero_rates.csv')
names(zero_rates) <- as.numeric(gsub('X', '', names(zero_rates)))
all_rates <- data.frame(tenor=names(zero_rates), zero=t(zero_rates), row.names=NULL, stringsAsFactors = F)
all_rates$tenor <- as.numeric(all_rates$tenor)

## Derived data.
all_rates$disc <- 1/((1+(all_rates$zero/100)) ** all_rates$tenor)
all_rates$fwd <- ((all_rates$disc)/lead(all_rates$disc) - 1)*100

## SHINY!!!
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
    cash_flows() * v$df$disc[1:length(cash_flows())]
  })
  ## All flows.
  all_flows <- reactive({
    data.frame(cash_flows=cash_flows(), disc_flows=disc_flows())
  })
  
  ## YTM.
  ytm <- reactive({
    f.ytm<-function(ytm) {
      sum(cash_flows() / (1+ytm) ** seq(length(cash_flows()))) - input$price
    }
    uniroot(f.ytm, interval=c(0,25))$root * 100
  })
  
  ## Spread.
  spread <- reactive({
    f.spread <- function(s) {
      sum(cash_flows() / ((1 + v$df$zero[1:length(cash_flows())]/100 + s) ** seq(length(cash_flows())))) - input$price
    }
    uniroot(f.spread, interval=c(0,25))$root * 100
  })

  ## Hazard Rate.  
  spread <- reactive({
    f.spread <- function(s) {
      sum(cash_flows() / ((1 + v$df$zero[1:length(cash_flows())]/100 + s) ** seq(length(cash_flows())))) - input$price
    }
    uniroot(f.spread, interval=c(0,25))$root * 100
  })
  
  
  ## Spread curve.
  zspread <- reactive({
    v$df$zero + spread()
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
    with(v$df, plot(tenor, zero, ylim=c(0, 10), xlab='Maturity (Y)', ylab='Yield (%)', pch=20, col='#000000'))
    with(v$df, lines(tenor, zero, pch=16, col='#000000'))
    with(v$df, lines(tenor, fwd, lty=2, col='#999999'))
    with(v$df, lines(tenor, zspread(), col='#428bca'))
    with(v$df, points(tenor, zspread(), pch=20, col='#428bca'))
    abline(h=ytm(), untf=FALSE, col='#a00000', lty="dotted")
    points(length(cash_flows()), ytm(), pch=20, col='#a00000')
    title('USD Govt Zero Coupon')
    legend(22, 10, c('YTM', 'Fwd Rates', 'Zero Rates'), lty=c(3,2,1), col=c('#a00000', '#999999', '#428bca'))
  })
  
  ## Plot cash flows.
  output$cash_flows <- renderPlot({
    barplot(t(as.matrix(all_flows())), names.arg=seq(v$maturity), beside=T, cols=c('#0a0','#000000'))
  })
  
  ## Output vars.
  output$ytm <- renderText({
    paste0('YTM=', ytm())
    paste0('SPD=', spread())
  })
})
