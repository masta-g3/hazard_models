library(shiny)

ui <- fluidPage(
  h1("Plot click demo"),
  plotOutput("plot", clickId = "plot_click"),
  actionButton("reset", "Reset zoom")
)

server <- function(input, output, session) {
  v <- reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    range = NULL    # After two clicks, this stores the range of x
  )

  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if (is.null(v$click1)) {
      # We don't have a first click, so this is the first click
      v$click1 <- input$plot_click
    } else {
      # We already had a first click, so this is the second click.
      # Make a range from the previous click and this one.
      v$range <- range(v$click1$x, input$plot_click$x)
      # And clear the first click so the next click starts a new
      # range.
      v$click1 <- NULL
    }
  })

  observeEvent(input$reset, {
    # Reset both the range and the first click, if any.
    v$range <- NULL
    v$click1 <- NULL
  })

  output$plot <- renderPlot({
    plot(cars, xlim = v$range)
    if (!is.null(v$click1$x))
      abline(v = v$click1$x)
  })
}

shinyApp(ui, server)
