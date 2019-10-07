## My R Console shiny script

library(shiny)
library(shinyjs)
library(data.table)
library(htmlTable)

## Function to create ui components for R console. Insert in ui object.
cmc_rconsole_ui <- function(str = "openrcon") {
  if(is.null(str) | is.na(str))
    str <- ""
  fluidPage(
    tags$img(src="https://www.r-project.org/logo/Rlogo.png", width = 50, height = 40, id="rtoggle"),
    tags$span(textInput("rpanel_pwd", NULL)),
    hidden(
      tags$div(style = "width: 100%; height: 600px; border: 3px solid #CCD;", id="rpanel",
               splitLayout(
                 verticalLayout(
                   splitLayout(cellWidths = c("25%", "75%"),
                               fluidPage(
                                 tags$h4("Console"),
                                 actionButton("rconsole_exec", "Run", height = "30px",
                                              style="background: #CCD; border: 2px #003; border-style: outset;"),
                                 checkboxInput("rconsole_hist", "Addon")
                                 # numericInput("rconsole_maxrows", "Max. Rows", min = 4, max = 200, value = 15)
                               ),
                               textAreaInput("rconsole", NULL, height = "85px")
                   ),
                   plotOutput("rcon_plot")
                 ),
                 tags$div(style=paste("width:100%; height:600px; overflow-y: scroll; overflow-x: wrap;",
                                      "border: 1px solid #003;"),
                          htmlOutput("rcon_text", style="font-family: Lucida Console;")
                 )
               )
      )
    ),
    tags$style('
      .rconsole_call hover {
          height: 100%;
          padding-bottom: opx;
          overflow: hidden;
      }
      .fixedcontainer {
          height: 200px;
          width: 100%;
          overflow-y: auto;
          overflow-x: auto;
          padding-right: 0px;
      }
      .fixedheader {
          font-size: 11pt;
      }
      .fixedheader thead tr th {
          //background: #447;
          background: #F5F5F5;
          color: #111;
          text-align: center;
          position: sticky;
          height: 20pt;
          top: 0;
          border-left: 3px solid #FFF;
      }
      .fixedheader tbody tr:nth-child(even) {
          background: #DDD;
      }
      .fixedtotal tbody tr:nth-last-child(1) td {
          background: #99B;
          position: sticky;
          bottom: 0;
      }
    '),
    tags$script(HTML(sprintf(
      '
      HTMLCollection.prototype.forEach = Array.prototype.forEach;

      // Function to change CSS top style attribute values to make headers fixed.
      function fixheaders(table) {
        var thead = table.getElementsByTagName("thead")[0];
        if(thead.getElementsByTagName("tr").length > 1) {
          var th_height = 0;
          for(i=0; i < thead.getElementsByTagName("tr").length; i++) {
            if(i > 0) {
              th_height += Number(getComputedStyle(thead.getElementsByTagName("tr")[i-1].children[0]).height.match(/[0-9\\.]+/g));
            }
            thead.getElementsByTagName("tr")[i].getElementsByTagName("th").forEach(function(item) {
              item.style.top = th_height + "px";
              if(item.innerHTML == "") {
                item.style.background = "#FFF";
              }
            })
          }
        }
      }
      window.onload = function() {
        $(".fixedheader").each(fixheaders);
      };
      
      // Keep track of whether Ctrl is pressed.
      var ctrldown = 0;
      
      //$(".fixedcontainer").resizable({handles: "s", minHeight: 200px, maxHeight: 400px});
      
      $("#rpanel_pwd").toggle();
      $("#rconsole_hist").css("padding", "1px");
      $("#rconsole_hist").css("margin-bottom", "1px");
      $("#rconsole_hist").css("margin-top", "1px");
      
      $("#rpanel_pwd").attr("type", "password");
      
      $("#rtoggle").click(function(e) {
        if($("#rpanel").css("display") == "none") {
          if($("#rpanel_pwd").val() == "%s") {
            shinyjs.show("rpanel");
            $("#rconsole").focus();
          } else {
            $("#rpanel_pwd").slideToggle("fast");
            $("#rpanel_pwd").focus();
          }
        } else {
          shinyjs.hide("rpanel");
        }
      })
      $("#rpanel_pwd").keyup(function(e) {
        if(e.which == 13 && $("#rpanel_pwd").val() == "%s") {
          shinyjs.show("rpanel");
          $("#rpanel_pwd").slideToggle("fast");
          $("#rpanel_pwd").val("");
          $("#rconsole").focus();
        }
      })
      $("#rpanel").keyup(function(e) {
        if(e.which == 17) {
          ctrldown = 0;
        }
      })
      $("#rconsole").keydown(function(e) {
        if(e.which == 17) {
          ctrldown = 1;
        }
        if(e.which == 13 && ctrldown == 1) {
          $("#rconsole_exec").click();
        }
      })
      $("#rconsole").keydown(function(e) {
        if(e.which == 27)
          $("#rtoggle").click();
      })
      $("#rconsole_exec").click(function(e) {
        $(".scrollcall").each(function() {
          this.style.paddingRight = (this.offsetWidth - this.clientWidth) + "px";
        });
        $(".fixedcontainer").each(function() {
          this.style.paddingBottom = (this.offsetHeight - this.clientHeight) + "px";
        });
      })
    ', str, str)))
  )
}

## Function to insert in server object.
cmc_rconsole_server <- function(input, output, session) {
  
  rconsole_output <- reactive({
    if(input$rconsole_exec == 0)
      rconsole_output <<- ""
  })
  
  observe({
    if(input$rconsole_exec > 0) {
      txt <- isolate(input$rconsole)
      res <- tryCatch(eval(parse(text = txt)), error = rconsole_funs$stop)
      # res <- tryCatch(eval(parse(text = txt), envir = globalenv()), error = rconsole_funs$stop)
      
      if(is.null(res)) {
        ## Assume that NULL output implies plotting
        
        ## Remove warnings()
        assign("last.warning", NULL, envir = baseenv())
        
        ## Evaluate plotting code
        output$rcon_plot <- renderPlot({
          eval(parse(text = txt))
          
          ## If the plotting includes warnings, print in console window
          if(!is.null(warnings())) {
            output$rcon_text <- renderText({
              if(length(warnings()) > 0) {
                tab <- data.table(Warning = names(warnings()), Call = sapply(warnings(), deparse))
                htmlTable(tab, rnames = F ,css.table = "color: #B66;", css.cell = rconsole_funs$padding(1), caption = "Warnings")
              }
            })
          }
        })
      } else if("gg" %in% class(res)) {
        output$rcon_plot <- renderPlot({
          res
        })
      } else {
        ## Print non-NULL output in console window
        output$rcon_text <- renderText({
          if(isolate(input$rconsole_hist))
            rconsole_output <<- paste(rconsole_output, HTML("<br>"),
                                     rconsole_funs$format_call(txt), rconsole_funs$format_output(res, input))
          else
            rconsole_output <<- paste(rconsole_funs$format_call(txt), rconsole_funs$format_output(res, input))
          
          rconsole_output
        })
      }
    }
  })
}

rconsole_funs <- list(
  
  padding = function(n = 1) {
    paste0("padding-right: ", n, "em; padding-left: ", n, "em;")
  },
  
  stop = function(err) {
    '<span style="font-weight: 900; color: #B66;">An error occurred...</span>'
  },
  
  format_call = function(txt) {
    tags$div(tags$strong("Call: "), txt, HTML("<br>"), style="color: #66C; width: 100%;", class="rconsole_call")
  },
  
  scroll_div = function(tab) {
    tab <- as.character(tags$div(HTML(tab), class = "fixedcontainer"))
    
  },
  
  format_array <- function(arr) {
    ## Format values to be the same character length
    if(mode(arr) == "numeric" | grepl("(&nbsp;|<table)", arr[1])==0)
      arr <- array(gsub("\\s", "&nbsp;", format(arr)), dim = dim(arr), dimnames = dimnames(arr))
    
    ## Set dimnames if NULL
    if(is.null(dimnames(arr)))
      dimnames(arr) <- lapply(dim(arr), function(n) 1:n)
    
    ## If array has odd number of dimensions, add dummy dimension for recursive call
    if(length(dim(arr)) %% 2 == 1) {
      arr <- array(arr, dim = c(dim(arr), 1), dimnames = c(dimnames(arr), ""))
    }
    
    ## Recurse if more than 2 dimensions
    if(length(dim(arr)) > 2) {
      format_array(apply(arr, tail(seq_along(dim(arr)), 2), format_array))
    } else {
      ## Create CSS for table
      css <- array(rconsole_funs$padding(.5), dim = dim(arr) + 1)
      css <- array(paste(ifelse(col(css) == 1, "font-weight: 900;", ""),
                      ifelse(row(css) == 2 & col(css) > 1, "border-top: 1px solid #222;", ""),
                      ifelse(col(css) == 2 & row(css) > 1, "border-left: 1px solid #222;", ""),
                      ifelse(row(css) == nrow(css), "border-bottom: 2px solid #222;", ""),
                      ifelse(col(css) == ncol(css) & row(css) > 1, "border-right: 1px solid #222;", ""),
                      # ifelse(row(css) == 1, paste0("width: ", ifelse(col(css) == 1, width1, width2), "em;"), ""),
                      css, sep = "" ), dim = dim(css))
      
      ## Create table to display
      tab <- htmlTable(arr, css.cell = css, css.table = "margin-top: .5em; margin-bottom: .5em; font-family: Lucida Console; border-left: 1px solid #222")
      tab <- gsub("(border-bottom: 1px solid grey;|border-top: 2px solid grey;)", "", tab)
      tab <- gsub("border-bottom: 2px solid grey;", "border-bottom: 1px solid #222;", tab)
      tab
    }
  },
  
  format_class = function(obj) {
    tags$span(tags$strong("Class: "), paste(class(obj), collapse = ", "),
              style=paste0("color: ", "8B7355", ";"))
  },
  
  format_mode = function(obj) {
    tags$span(tags$strong("Mode: "), paste(mode(obj), collapse = ", "),
              style=paste0("color: ", "#CDAA7D", ";"))
  },
  
  format_dim = function(obj) {
    tags$span(tags$strong(ifelse(is.null(dim(obj)), "Length: ", "Dimension: ")),
              paste(ifelse(is.null(dim(obj)), length(obj), paste(dim(obj), collapse = "x")), collapse = ", "),
              style=paste0("color: ", "#CDAA7D", ";"))
  },
  
  format_key = function(obj) {
    tags$span(tags$strong("Table Key: "),
              ifelse(is.null(key(obj)), "--", paste(key(obj), collapse = ", ")),
              style=paste0("color: ", "#CDAA7D", ";"))
  },
  
  format_output = function(res, input) {
    
    res_class = rconsole_funs$format_class(res)
    res_mode = rconsole_funs$format_mode(res)
    res_dim = rconsole_funs$format_dim(res)
    
    if(is.data.frame(res)) {
      ## Format data frames
      
      ## Create caption
      ##  Print class
      ##  Print dimension
      ##  Print table key (data.table objects only)
      if(is.data.table(res))
        caption = paste0(res_class, "<br>", res_dim, "<br>", rconsole_funs$format_key(res))
      else
        caption = paste0(res_class, "<br>", res_dim)
      
      res <- htmlTable(res, css.cell = rconsole_funs$padding(1), 
                       align = "r", caption = caption, css.class = "fixedheader" )
      res <- rconsole_funs$scroll_div(res)
      
    } else if (is.list(res)) {
      ## Format lists (recursive)
      
      if(class(res) == "reactivevalues")
        res <- reactiveValuesToList(res)
      
      ## Recursive call for list elements
      res <- lapply(res, rconsole_funs$format_output, input = input)
      
      ## Add numbers for missing names
      if(is.null(names(res)))
        names(res) <- rep("", length(res))
      lst_names <- ifelse(names(res) == "", paste0("[[", seq_along(res), "]]"), paste0("$", names(res)))
      
      ## Chain together
      res <- paste0("<strong><span hidden></span></strong>:<br>", res)
      res <- sapply(seq_along(res), function(i) gsub("<span hidden></span>", paste0("<span hidden></span>", lst_names[i]), res[[i]]))
      res <- paste0(res, collapse = "<br><br>")
      
      res <- paste0(res_class, "<br>", res_dim, "<br>", res)
      
    } else if (class(res) %in% c("numeric", "integer", "character", "logical") & length(res) > 1) {
      ## Format vectors
      res <- htmlTable(data.table(Name = names(res), Value = res),
                       css.cell = rconsole_funs$padding(2),
                       align = ifelse(class(res) == "character", "ll", "lr"),
                       caption = paste0(res_class, "<br>", res_dim), css.class = "fixedheader")
      res <- rconsole_funs$scroll_div(res)
      
    } else if (is.factor(res) & length(res) > 1) {
      ## Format factors (side-by-side tables for the vector elements and factor levels)
      res <- cbind(Vector = htmlTable(data.table(Name = names(res), Value = res),
                                      css.cell = rconsole_funs$padding(2),
                                      caption = res_dim, css.class = "fixedheader"),
                   Factor = htmlTable(data.table(Levels = levels(res)),
                                      css.cell = rconsole_funs$padding(2),
                                      caption = rconsole_funs$format_dim(levels(res))) )
      res <- htmlTable(res, rnames = F, caption = res_class,
                       css.cell = paste(rconsole_funs$padding(2), "vertical-align: top;"))
      
    } else if (is.array(res)) {
      ## Format arrays/matrices
      res <- format_array(res)
      res <- htmlTable(res, rnames = F, caption = paste0(res_class, "<br>", res_mode, "<br>", res_dim))
      
    } else if (is.function(res)) {
      ## Format functions
      res <- paste(head(res, Inf), collapse = "<br>")
      res <- gsub("\\s", "&nbsp;", res)
      res <- paste0(res_class, "<br>", res)
      res <- rconsole_funs$scroll_div(res)
      
    }
    
    res
  }
)

