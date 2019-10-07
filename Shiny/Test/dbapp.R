# Test R console within shiny app
 library(CMcCode)
 library(cli)
 library(data.table)
 library(ggplot2)
 library(htmlTable)
 library(Hmisc)
 library(mgcv)
 library(shiny)
 library(shinydashboard)
 library(shinyjs)
 library(tweedie)
 
 # rm(list = ls())
 
 setwd("/home/cmcneil/Projects/shiny/Test")
 source("shiny_rconsole.R")
 
 ggthm <- theme(panel.background = element_rect(fill = "white"), text = element_text(size = 15))
 
 # spinner <- make_spinner("circleHalves")
 spinner <- get_spinner("circleHalves")$frames
 mydt <- data.table(x = rnorm(5000))[, y := rnorm(.N, 30 + 10*x, 5)]
 dists <- c("normal", "gamma" ,"beta", "binomial", "negbinomial", "tweedie")
 names(dists) <- (replace(capitalize(dists), dists == "negbinomial", "Negative Binomial"))
 
 vals <- reactive({})
 
 ## List of HTML greek letters
 symbols <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "kappa",
              "lambda", "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "phi", "chi", "psi", "omega")
 symbols <- c(symbols, capitalize(symbols))
 names(symbols) <- symbols
 symbols <- sapply(symbols, sprintf, fmt = "&%s;")
 symbols <- lapply(symbols, HTML)
 
 ## Shiny UI
 ui <- dashboardPage(skin = "green",
   dashboardHeader(title = "Shiny", titleWidth = 200),
   dashboardSidebar(width = 200,
     sidebarMenu(
       menuItem("Main page", tabName = "main", icon = icon("home")),
       menuItem("Basic Shiny", tabName = "old", icon = icon("clock")),
       menuItem("R Console", tabName = "console", icon = icon("wrench"))
     )
   ),
   dashboardBody(
     tags$head( tags$style( HTML("info-box-text .fa, .glyphicon { font-size: 30px; }") ) ),
     tabItems(
       tabItem(tabName = "main",
         # flowLayout(
           fluidRow(
             box(width = 6, 
               sliderInput("numpoints", "Sample size", value = 100, min = 50, max = 2000)
             ),
             box(width = 6, 
               selectInput("distribution", "Distribution", selected = "normal", choices = dists)
             )
           ),
           fluidRow(
             box(width = 4,
               uiOutput("param1")
             ),
             box(width = 4,
               uiOutput("param2")
             ),
             box(width = 4,
               uiOutput("param3")
             )
           ),
           fluidRow(
             column(width = 8,
               plotOutput("bigplot")
             ),
             column(width = 4,
               infoBoxOutput("drawmin", width = NULL),
               infoBoxOutput("drawmean", width = NULL),
               infoBoxOutput("drawmax", width = NULL),
               infoBoxOutput("drawpcnt", width = NULL)
             )
           )
         # )
       ),
       tabItem(tabName = "old", style="background: white;",
         fluidPage( 
           # App title
           useShinyjs(),
           titlePanel("Basic App"),
           
           # Sidebar
           sidebarLayout(
             sidebarPanel(
               sliderInput("numobs", label = "Number of Observations:", min = 10, max = nrow(mydt), value = 100),
               
               textInput("mytext", "Type text ... it won't do anything:", width = "200px")
             ),
             
             mainPanel(
               plotOutput("plot"),
               textOutput("envir")
             )
           )
           
           # cmc_rconsole_ui(str = getPassword())
         )
       ),
       tabItem(tabName = "console",
         cmc_rconsole_ui(str = getPassword())
       )
     )
   )
 )
 
 
 observe({
   print(spinner[1])
   spinner <<- c(spinner[-1], spinner[1])
   spinSpinner()
 })
 
 spin <- function(err) {
   spinSpinner <<- reactiveTimer(100)
   # spinner$spin()
   # # Sys.sleep(.05)
   # reactiveTimer(.05)
   # plot(1, main = ansi_with_hidden_cursor(paste("Waiting ", spinner), stream = stdout()))
   spinner <<- c(spinner[-1], spinner[1])
   plot(1, 1, axes = F, type = "n", xlab = "", ylab = "")
   text(1, 1, spinner[1], cex = 15)
   spinSpinner()
 }
 
 ## Shiny Server function
 server <- function(input, output, session) {
   output$bigplot <- renderPlot({tryCatch({
     dist <- input$distribution
     tmpvals <- vals()
     if(exists("spinSpinner"))
       rm(spinSpinner)
     
     ## Plot histogram (binomial distribution modified for integers)
     if(dist == "binomial")
       hist(tmpvals, breaks = input$binomial_size, freq = F)
     else
       hist(tmpvals, breaks = 50, freq = F)
     
     ## Change to integers if the same (used for Tweedie distribution with p = 1)
     # if(all(tmpvals == as.integer(tmpvals)))
     #   tmpvals <- as.integer(tmpvals)
     # switch(class(tmpvals),
     #        numeric = hist(tmpvals, fill = "blue", breaks = 50, freq = F, ylab = "Density"),
     #        integer = arm::discrete.histogram(tmpvals))
     
     ## Choose sequence of points based on distribution; evaluate and plot density function
     xseq <- switch(dist,
                    normal = ,
                    tweedie = ,
                    gamma = seq(min(tmpvals), max(tmpvals), length.out = 100),
                    beta = seq(0, 1, length.out = 100),
                    binomial = seq(0, input$binomial_size, by = 1),
                    negbinomial = seq(0, max(tmpvals), by = 1))
     dens <- switch(dist,
                    normal = dnorm(xseq, input$normal_mean, input$normal_sd),
                    gamma = dgamma(xseq - input$gamma_shift, input$gamma_shape, input$gamma_scale),
                    beta = dbeta(xseq, input$shape1, input$shape2),
                    binomial = dbinom(xseq, input$binomial_size, input$binomial_prob),
                    negbinomial = dnbinom(xseq, size = input$negbinomial_size, prob = input$negbinomial_prob),
                    tweedie = dtweedie(xseq, mu = input$tweedie_mu, phi = input$tweedie_phi, power = input$tweedie_power))
     
     ## Evaluate and plot distribution over histogram
     tmpdt <- data.table(x = xseq, y = dens)
     if(is.integer(tmpvals))
       ggplot(data.table(Val = tmpvals)) +
         geom_histogram(aes(Val, y = ..density..), col = "white", fill = "blue", binwidth = 1) +
         geom_line(data = tmpdt, aes(x, y), size = 1, col = "indianred") + ggthm +
         labs(title = paste("Sample from", capitalize(input$distribution), "Distribution"))
     else
       ggplot(data.table(Val = tmpvals)) +
         geom_histogram(aes(Val, y = ..density..), col = "white", fill = "blue") +
         geom_line(data = tmpdt, aes(x, y), size = 1, col = "indianred") + ggthm +
         labs(title = paste("Sample from", capitalize(input$distribution), "Distribution"))
   },
   error = spin)})
   
   output$drawmin <- renderInfoBox({
     distmin = round(min(vals()), 2)
     if(exists("spinSpinner"))
       spinSpinner()
     infoBox("Min", ifelse(is.na(distmin), spinner[1], distmin), icon = icon("step-backward", lib = "glyphicon"))
   })
   output$drawmax <- renderInfoBox({
     distmax = round(max(vals()), 2)
     if(exists("spinSpinner"))
       spinSpinner()
     infoBox("Max", ifelse(is.na(distmax), spinner[1], distmax), icon = icon("step-forward", lib = "glyphicon"))
   })
   output$drawmean <- renderInfoBox({
     distmean = round(mean(vals()), 2)
     if(exists("spinSpinner"))
       spinSpinner()
     infoBox("Mean", ifelse(is.na(distmean), spinner[1], distmean), icon = icon("balance-scale"), width = 2)
   })
   output$drawpcnt <- renderInfoBox({
     distpcnt = sprintf("%.02f%%", mean(vals()==0))
     if(exists("spinSpinner"))
       spinSpinner()
     infoBox("% Zeros", ifelse(is.na(distpcnt), spinner[1], distpcnt), icon = icon("percent"))
   })
   
   ## Render output for Old Shiny tab
   output$plot <- renderPlot({
     ggplot(mydt[1:min(.N, input$numobs)], aes(x, y)) + geom_point()
   })
   output$envir <- renderText({
     input$rexecute
     as.character(environment(input))
   })
   
   observe({
     dist <- input$distribution
     print("Here")
     if(dist == "normal") {
       print("Here too")
       output$param1 <- renderUI({numericInput("normal_mean", label = symbols$mu, value = 0, min = -100, max = 100)})
       output$param2 <- renderUI({numericInput("normal_sd", label = symbols$sigma, value = 1, min = 0.05, max = 100, step = .05)})
       output$param3 <- renderUI({""})
     } else if(dist == "gamma") {
       output$param1 <- renderUI({numericInput("gamma_shape", label = symbols$alpha, value = 1, min = 0.05, max = 100, step = .05)})
       output$param2 <- renderUI({numericInput("gamma_scale", label = symbols$beta, value = 1, min = 0.05, max = 100, step = .05)})
       output$param3 <- renderUI({numericInput("gamma_shift", label = symbols$delta, value = 0, min = -100, max = 100)})
     } else if(dist == "beta") {
       output$param1 <- renderUI({numericInput("beta_shape", label = symbols$alpha, value = 1, min = 0, max = 100)})
       output$param2 <- renderUI({numericInput("beta_scale", label = symbols$beta, value = 1, min = 0, max = 100)})
       output$param3 <- renderUI({""})
     } else if(dist == "binomial") {
       output$param1 <- renderUI({numericInput("binomial_size", label = "n", value = 1, min = 1, max = 100, step = 1)})
       output$param2 <- renderUI({numericInput("binomial_prob", label = "p", value = .5, min = 0, max = 1)})
       output$param3 <- renderUI({""})
     } else if(dist == "negbinomial") {
       output$param1 <- renderUI({numericInput("negbinomial_size", label = "n", value = 1, min = 1, max = 100, step = 1)})
       output$param2 <- renderUI({numericInput("negbinomial_prob", label = "p", value = .5, min = 0, max = 1)})
       output$param3 <- renderUI({""})
     } else if(dist == "tweedie") {
       output$param1 <- renderUI({numericInput("tweedie_power", label = "p", value = 1.25, min = 1, max = 2, step = .05)})
       output$param2 <- renderUI({numericInput("tweedie_mu", label = symbols$mu, value = 5, min = 0, max = 100)})
       output$param3 <- renderUI({numericInput("tweedie_phi", label = symbols$phi, value = 1, min = 0, max = 100)})
     }
   })
   
   vals <- reactive({
     dist <- input$distribution
     if(dist == "normal") {
       rnorm(input$numpoints, mean = input$normal_mean, sd = input$normal_sd)
     } else if (dist == "gamma") {
       rgamma(input$numpoints, shape = input$gamma_shape, scale = input$gamma_scale) + input$gamma_shift
     } else if (dist == "beta") {
       rbeta(input$numpoints, shape1 = input$beta_shape, shape2 = input$beta_scale)
     } else if (dist == "binomial") {
       rbinom(input$numpoints, size = input$binomial_size, prob = input$binomial_prob)
     } else if (dist == "negbinomial") {
       rnbinom(input$numpoints, size = input$negbinomial_size, prob = input$negbinomial_prob)
     } else if (dist == "tweedie") {
       rtweedie(input$numpoints, power = input$tweedie_power, phi = input$tweedie_phi, mu = input$tweedie_mu)
     }
       
     # get(dist)(input$numpoints, input$param1, input$param2)
   })
    
   cmc_rconsole_server(input, output, session)
 }
 
 shinyApp(ui, server)
 
 
# End script
 
 