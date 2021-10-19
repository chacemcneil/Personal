library(shiny)

if (interactive()) {
  options(device.ask.default = FALSE)
  
  minval <- 40
  maxval <- 70
  ticks <- seq(minval, maxval, by = 1)
  ## Create desired labels for ticks
  labels <- ifelse(ticks == maxval, paste0(maxval, "+"), ticks)
  ## Create shiny input object
  html   <- sliderInput("age", "Average age:",
                        min = min(ticks), max = max(ticks), step=1, value = "0")
  ## Modify 'data-values' to change labels.
  ## This will cause the input to be interpreted on a different scale {0, 1, maxval-minval}
  html$children[[2]]$attribs[['data-values']] <- paste0(labels, collapse = ",")
  
  ui <- fluidPage(
    html,
    plotOutput("distPlot"),
    htmlOutput("text")
  )
  
  # Server logic
  server <- function(input, output) {
    age <- reactive({input$age + minval})
    output$distPlot <- renderPlot({
      hist(rnorm(1e3, age(), 5), breaks = 50)
      abline(v = age())
    })
    output$text <- renderText({
      paste0("The input for age is ", input$age, ", and has class ", class(input$age),
             ". The reactive statement in the code adds <code>minval</code> so that ",
             "<code>age()</code> returns ", age())
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}

