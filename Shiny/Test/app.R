# Test R console within shiny app
 library(data.table)
 library(ggplot2)
 library(htmlTable)
 library(shiny)
 library(shinyjs)
 
 setwd("/home/cmcneil/Projects/shiny/Test")
 source("shiny_rconsole.R")
 
 mydt <- data.table(x = rnorm(100))[, y := rnorm(.N, 30 + 10*x, 5)]
 
 ## Front end
 ui <- fluidPage( 
   # App title
   useShinyjs(),
   titlePanel("Test R Console"),
   
   # Sidebar
   sidebarLayout(
     sidebarPanel(
       sliderInput("maxobs", label = "Max Number of Observations:", min = 100, max = 5000, value = 500),
       
       textInput("mytext", "Explore R workspace by typing R code:", width = "200px")
     ),
     
     mainPanel(
       plotOutput("plot"),
       textOutput("envir")
     )
   ),
   cmc_rconsole_ui(str = "pass")
 )
 
 ## Back end
 server <- function(input, output, session) {
   
   output$plot <- renderPlot({
     # hist(rnorm(input$bins), main = paste(input$text, "with", input$bins, "observations"))
     ggplot(mydt[1:min(.N, input$maxobs)], aes(x, y)) + geom_point()
   })
   output$envir <- renderText({
     input$rexecute
     as.character(environment(input))
   })
   
   cmc_rconsole_server(input, output, session)
 }
 
 shinyApp(ui, server)
 
 
# End script
 
 