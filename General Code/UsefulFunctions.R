# Some useful functions
 library(data.table)
 
 
 upd <- function() {
   rm(lv,env,envir=.GlobalEnv)
   
   makeActiveBinding("env",environment,parent.env(environment()))
   makeActiveBinding("lv",function(x) .Last.value,env)
   
   options(width=250)
 }
 
 
 ini <- function() {
   cat(paste(readLines("/home/cmcneil/.odbc.ini"),collapse="\n"))
 }
 
 
 #rfiles <- dir("../Miscellaneous/",pattern=".R$",full.names=T)
 #rcode  <- lapply(rfiles,readLines)
 
 
 
 dfSummary <- function(...,table.names=NULL,track=T) {
   tabs <- list(...)
   if(is.null(names(tabs))) {
     if(is.null(table.names))
       names(tabs) <- paste0("Table",1:length(tabs))
     else
       names(tabs) <- table.names
   }
   summary <- do.call(rbind,lapply(seq_along(tabs),function(i) {
     if(track)
       cat(paste0("\rTable: ",names(tabs)[i]))
     dat <- tabs[[i]]
     column <- data.table(TableName=names(tabs)[i],
                          ColName=colnames(dat),
                          Class=sapply(dat,class),
                          Mode=sapply(dat,mode),
                          NumNA=sapply(dat,function(col) sum(is.na(col))),
                          PctNA=sapply(dat,function(col) mean(is.na(col))),
                          NumUnq=sapply(dat,function(col) length(unique(col))),
                          Length=nrow(dat),
                          NumLevels=sapply(dat,function(x) length(levels(x))))
     column[Mode=="numeric" & Class!="factor",Min                    :=sapply(dat[,ColName,with=F],min,na.rm=T)]
     column[Mode=="numeric" & Class!="factor" & Class!="Date",Qrt1   :=sapply(dat[,ColName,with=F],quantile,.25,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Median                 :=sapply(dat[,ColName,with=F],median,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Mean                   :=sapply(dat[,ColName,with=F],mean,na.rm=T)]
     column[Mode=="numeric" & Class!="factor" & Class!="Date",Qrt3   :=sapply(dat[,ColName,with=F],quantile,.75,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Max                    :=sapply(dat[,ColName,with=F],max,na.rm=T)]
     column[Mode=="numeric" & Class!="factor" & Class!="factor",Sum  :=sapply(dat[,ColName,with=F],sum,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Nonzero:=sapply(dat[,ColName,with=F],function(x) sum(x!=0,na.rm=T))]
     column[,Most:=sapply(dat,function(col) names(sort(table(col),decreasing=T))[1])]
     column[,MostCount:=sapply(dat,function(col) sort(table(col),decreasing=T)[1])]
     column[,MostUnique:=sapply(dat,function(col) diff(sort(table(col),decreasing=T)[1:2])!=0)]
     column[,Least:=sapply(dat,function(col) names(sort(table(col)))[1])]
     column[,LeastCount:=sapply(dat,function(col) sort(table(col))[1])]
     column[,LeastUnique:=sapply(dat,function(col) diff(sort(table(col))[1:2])!=0)]
     return(column)
   }))
   cat("\n")
   return(summary)
 }
 
 
 locateIndex <- function(dat,gg=F,print=T,n=1,...) {
   require(ggplot2)
   if(sum(!c("x","y") %in% colnames(dat))) {
     colnames(dat)[1:2] <- c("x","y")
   }
   if(gg)
     ggplot() + geom_point(data=dat,aes(x,y))
   else
     plot(dat$x,dat$y,...)
   
   if(gg) { # Not possible?
     gglocator(n)
   }
   else {
     coord <- data.frame(locator(n))
     ind <- NULL
     for (i in 1:nrow(coord)) {
       dist <- (dat[,1]-coord$x[i])^2 + (dat[,2]-coord$y[i])^2
       ind[i] <- which.min(dist)
     }
   }
   points(dat[ind,],col="red",pch=19)
   if(print)
     print(dat[ind,])
   return(ind)
 }
 
 #locateIndex(df <- data.frame(x=rnorm(100),y=rnorm(100)))
 
 
 getPassword <- function() {
   require(shiny)
   if(!file.exists("server.R")) {
     serverscript <- "
library(shiny)
shinyServer(function(input, output,session) {
    observe({
        input$button
        password <<- isolate(input$password)
    })
})
     "
     write(serverscript,file="server.R")
   }
   if(!file.exists("ui.R")) {
     uiscript <- "
library(shiny)
shinyUI(
    sidebarPanel(textInput(\"password\",\"Enter password: \"),actionButton(\"button\",\"Enter\"))
)
     "
     write(uiscript,file="ui.R")
   }
   runApp()
 }
 
#  getPassword <- function() {
#    require(tcltk)
#    tt <- tktoplevel() 
#    Password <- tclVar("") 
#    password <- NULL
#    tkf <- tkframe(tt,relief="ridge",borderwidth=3)
#    ## Frame options:
#    # borderwidth: width of border
#    # relief:      style of border
#    
#    entry.Password <-tkentry(tkf,width="20",textvariable=Password,show="*") 
#    tkgrid(tklabel(tkf,text="Please enter your password.")) 
#    tkgrid(entry.Password) 
#    OnOK <- function() 
#    { 
#      tkdestroy(tt)  
#      password <<- tclvalue(Password) 
#    } 
#    OK.but <-tkbutton(tt,text="   OK   ",command=OnOK) 
#    tkbind(entry.Password, "<Return>",OnOK) 
#    tkgrid(tkf,OK.but)
#    
#    while(is.null(password))
#    {}
#    tkdestroy(tt)
#    return(password)
#  }
 
 
 # Update savvy package so that password is not required
 if(exists("read.odbc")) {
   read.odbc <- function (dbString, dbTable, dbQuery = paste("SELECT * FROM", dbTable), trimStrings = TRUE, ...) 
   {
     dbChannel <- odbcChannel2(dbString)
     df <- sqlQuery(dbChannel, dbQuery, ...)
     odbcClose(dbChannel)
     if (class(df) != "data.frame") 
       warning("Possible error detected. Print returned object for assistance with diagnosis.\n  Is the table you requested in the database you specified?")
     if (trimStrings) {
       facs <- names(which(sapply(df, is.factor)))
       for (i in seq_along(facs)) levels(df[[i]]) <- str_trim(levels(df[[i]]))
     }
     df
   }
   
   odbcChannel2 <- function(dbString, dbUser = Sys.getenv("USER"), domain = "SAVVYSHERPA") {
     dsnNames <- names(odbcDataSources())
     dbName <- grep(dbString, dsnNames, ignore.case = TRUE, value = TRUE)
     if (length(dbName) == 0) 
       stop("The string \"", dbString, "\" was not found in the list of available data sources.\nCheck your odbc.ini file, or call odbcDataSources() to see the available list.")
     if (length(dbName) > 1) 
       stop("The string \"", dbString, "\" matches more than one of the available data sources.\nCheck your odbc.ini file, or call odbcDataSources() to see the available list.")
     dbFullUser <- paste(domain, "\\", dbUser, sep = "")
     if (exists("passEnv")) {
       if ((!exists("passEnv") && !exists("dbPassword", where = .GlobalEnv))) {
         stop("Please use set_db_password() to set your database password.")
       }
       if (exists("passEnv")) 
         dbPassword <- get("dbPassword", pos = passEnv)
       channel <- odbcConnect(dbName, dbFullUser, pwd = dbPassword, 
                              readOnlyOptimize = TRUE)
     }
     else {
       channel <- odbcConnect(dbName, readOnlyOptimize = TRUE)
     }
     if (is.null(attr(channel, "id")) || attr(channel, "id") < 
           0) 
       stop("Unable to connect to database, do you have the proper permissions?")
     channel
   }
   
 }
 
 
# end script
 