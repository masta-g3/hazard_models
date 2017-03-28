library(shiny)

shinyUI(fluidPage(theme = "minimal.css",
  h2('Interactive Yield and Hazard Calculator', align='center'),
  fluidRow(
    column(9,
      plotOutput('cash_flows')),
    column(3,
      numericInput('price', 'Price:', 80, min=0.1),
      sliderInput('maturity', 'Time to Maturity:', 10, min=0, max=30),
      sliderInput('coupon', 'Coupon:', 5, min=0, max=20),
      sliderInput('recovery', 'Recovery Rate:', 0.4, min=0, max=1)
    )
  ),
  fluidRow(
    column(8, 
      h3('To Do:'),
      div(HTML('<p><strike>1. Calculate forward rates.</strike></p>')),
      div(HTML('<p><strike>2. Calculate yield to maturity and spread.</strike></p>')),
      div(HTML('<p><strike>3. Calculate discounted cashflows (traditional method).</strike></p>')),
      p('4. Calculate constant hazard rate.'),
      p('5. Calculate discounted cashflows (hazard method).'),
      textOutput('ytm')
    ),
    column(4,
      plotOutput('risk_free', click = 'plot_click')
    )
  )
))