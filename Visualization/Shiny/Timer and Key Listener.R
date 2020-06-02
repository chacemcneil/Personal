# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)

# options(shiny.host = "192.168.0.5")
options(shiny.port = 8888)

# Use iris data
this <- reactiveVal(0)
posx <- reactiveVal(0)
posy <- reactiveVal(0)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Google Trend Index"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select Species
                    selectInput(inputId = "type", label = strong("Select Species"),
                                choices = unique(irissub$Species),
                                selected = "virginica"),
                    
                    # Select date range to be plotted
                    actionButton("button", "Press this"),
                    actionButton("left", "Left"),
                    actionButton("right", "Right"),
                    actionButton("down", "Down"),
                    actionButton("up", "Up")
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "buttontxt"),
                    textOutput(outputId = "buttontxt2"),
                    time_UI("timer"),
                    plotOutput("grid")
                  )
                )
)

# Define server function
server <- function(input, output, session) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    if(input$button > 0)
      ggplot(irissub[iris$Species == input$type,][1:10,], aes(Petal.Length, Petal.Width)) +
        geom_point()
    else
      ggplot(irissub[iris$Species == input$type,], aes(Petal.Length, Petal.Width)) +
        geom_point()
  })
  
  # Pull in description of trend
  output$buttontxt <- renderText({
    paste("The button has been pressed", input$button,
          ifelse(input$button == 1, "time.", "times."))
  })
  output$buttontxt2 <- renderText({
    paste("The button has been pressed", this(),
          ifelse(this() == 1, "time.", "times."))
  })
  observeEvent(input$button, {
    newval <- this() + 1
    this(newval)
  })
  
  observeEvent(input$left, {
    newval <- posx() - 1
    posx(newval)
  })
  observeEvent(input$right, {
    newval <- posx() + 1
    posx(newval)
  })
  observeEvent(input$down, {
    newval <- posy() - 1
    posy(newval)
  })
  observeEvent(input$up, {
    newval <- posy() + 1
    posy(newval)
  })
  
  timer <- callModule(module = time, id = "timer", start = reactive(input$button))
  
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  
  output$grid <- renderPlot({
    plot(0, xlim = c(-10, 10), ylim = c(-10, 10), type = "n")
    points(posx(), posy(), cex = 5, pch = 19)
  })
  
}


## Time module ----------------------------

time_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "width: 100%; text-align: center;",
    "Time elapsed:",
    uiOutput(outputId = ns("timer_ui"), style = "font-size: 200%; font-weight: bold;", inline = TRUE)
  )
}

time_r <- reactiveVal(value = 0)

time <- function(input, output, session, start = reactive(0)) {
  
  started <- reactiveVal(value = FALSE)
  
  observeEvent(start(), {
    time_r(0)
    started(TRUE)
  }, ignoreInit = TRUE)
  
  observe({
    if (started()) {
      invalidateLater(1000, session)
      isolate({
        newTime <- time_r() + 1
        time_r(newTime)
      })
    }
  })
  
  output$timer_ui <- renderUI({
    as.character(time_r())
  })
  
  return(time_r)
}

# Create Shiny object
shinyApp(ui = ui, server = server)



