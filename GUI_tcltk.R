# Exploring GUIs
 library(tcltk) 
 
 # Website for examples: http://www.sciviews.org/_rgui/tcltk/
 
 
 
 ### Password (hidden text, bind key action, button)
 
 tt<-tktoplevel(bg="seagreen") 
 PW <- tclVar("") # Needed for reading value
 printPW <- tclVar("N")
 password <- NULL
 tkf  <- tkframe(tt,relief="ridge",borderwidth=3) # relief is type of border (raised, sunken, flat, ridge, solid, and groove),
 tkf2 <- tkframe(tt,relief="raised",borderwidth=3) # borderwidth is ...
 tkf3 <- tkframe(tkf2,relief="sunken",borderwidth=1)
 
 #entry.Password <-tkentry(tkf,width="20",textvariable=Password,show="*") 
 entry.Password <-tkentry(tkf,width="20",text="",textvariable=PW,show="*") 
 invisible(tkgrid(tklabel(tkf,text="Please enter your password.")) )
 invisible(tkgrid(entry.Password) )
 rb1 <- tkradiobutton(tkf3,variable=printPW,value="Y")
 rb2 <- tkradiobutton(tkf3,variable=printPW,value="N")
 tkgrid(tklabel(tkf2,text="Echo password?"))
 tkgrid(tkf3)
 tkgrid(tklabel(tkf3,text="Yes "),rb1,tklabel(tkf3,text="  No "),rb2)
 OnOK <- function() 
 { 
   tkdestroy(tt)  
   #password <<- tclvalue(Password) 
   password <<- tclvalue(PW) 
   if(tclvalue(printPW)=="Y")
     tkmessageBox(message=paste("The password you entered is:",password))
 } 
 OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)  # command attaches function performed when clicked
 invisible(tkbind(entry.Password, "<Return>",OnOK) ) # bind the return key to the tkentry object
 invisible(tkgrid(tkf,tkf2,OK.but))                       # add frame and button to their parent frame (tt)
 
 warning("\r        \n\nEnter password in other window ...\n\n",immediate.=T,call.=F)
 while(is.null(password))
 {}
 print(password)
 tkdestroy(tt)
 
 ### Radio Buttons
 
 require(tcltk)
 tt <- tktoplevel()
 rb1 <- tkradiobutton(tt)
 rb2 <- tkradiobutton(tt)
 rbValue <- tclVar("oranges")
 tkconfigure(rb1,variable=rbValue,value="apples")
 tkconfigure(rb2,variable=rbValue,value="oranges")
 tkgrid(tklabel(tt,text="Which do you prefer?"))
 tkgrid(tklabel(tt,text="Apples "),rb1)
 tkgrid(tklabel(tt,text="Oranges "),rb2)
 
 OnOK <- function()
 {
   rbVal <- as.character(tclvalue(rbValue))
   tkdestroy(tt)
   if (rbVal=="apples")
     tkmessageBox(message="Good choice!  An apple a day keeps the doctor away!")
   if (rbVal=="oranges")
     tkmessageBox(message="Good choice!  Oranges are full of Vitamin C!")
 }
 OK.but <- tkbutton(tt,text="OK",command=OnOK)
 tkgrid(OK.but)
 tkfocus(tt)
 
 ### Frame arrangement
 
 require(tcltk)
 tt <- tktoplevel()
 frameOverall <- tkframe(tt)
 frameUpper <- tkframe(frameOverall,relief="groove",borderwidth=2)
 tkgrid(tklabel(frameUpper,text="Text in the upper frame"))
 frameLower <- tkframe(frameOverall,relief="groove",borderwidth=2)
 tkgrid(tklabel(frameLower,text="Text in the lower frame"))
 
 tkgrid(frameUpper)
 tkgrid(tklabel(frameOverall,text="Text between the upper and lower frames"))
 tkgrid(frameLower)
 tkgrid(frameOverall)
 
 ### Odd frame arrangement
 
 tt <- tktoplevel()
 overall <- tkframe(tt)
 topframe <- tkframe(overall)
 botframe <- tkframe(overall)
 frame1 <- tkframe(topframe)
 rightframe <- tkframe(topframe)
 frame2 <- tkframe(rightframe)
 frame3 <- tkframe(rightframe)
 frame4 <- tkframe(rightframe)
 frame5 <- tkframe(botframe)
 frame6 <- tkframe(botframe)
 
 mysum <- function() {
   return(1)
 }
 tkgrid(tkbutton(frame1,text="  Done  ",command=mysum))
 tkgrid(tklabel(frame2,text="2"))
 tkgrid(tklabel(frame3,text="3"))
 tkgrid(tklabel(frame4,text="4"))
 tkgrid(tklabel(frame5,text="5"))
 tkgrid(tklabel(frame6,text="6"))
 
 tkgrid(frame2)
 #tkgrid(frame3,frame4,frame6) # error if not added to parent frame
 tkgrid(frame3,frame4)
 tkgrid(frame1,rightframe)
 tkgrid(frame5,frame6)
 tkgrid(topframe)
 tkgrid(botframe)
 tkgrid(overall)
 
 ### Slider
 
 require(tcltk)
 tt <- tktoplevel()
 SliderValue <- tclVar("50")
 SliderValueLabel <- tklabel(tt,text=as.character(tclvalue(SliderValue)))
 tkgrid(tklabel(tt,text="Slider Value : "),SliderValueLabel,tklabel(tt,text="%"))
 tkconfigure(SliderValueLabel,textvariable=SliderValue)
 slider <- tkscale(tt, from=100, to=0,
                   showvalue=T, variable=SliderValue,
                   resolution=1, orient="vertical")
 tkgrid(slider)
 tkfocus(tt)
 
 
 ### Add R plots
 require(tkrplot)
 require(ggplot2)
 tt <- tktoplevel(bg="white")
 tkf <- tkframe(tt,bg="blue")
 bb<-1
 img <-tkrplot(tt, function() {plot(1:20,(1:20)^bb,xlab="x",ylab="y")})
 img <-tkrplot(tt, function() {hist(c(1:20,(1:20)^bb))})
 img <-tkrplot(tt, function() {print(ggplot(data.frame(x=1:20,y=(1:20)^bb),aes(x,y))+geom_point())})
 tkbind(img,"<Double-Button-1>",function() {bb <<- bb+.1;tkrreplot(img)})
 f<-function(...) {
   b <- as.numeric(tclvalue("bb"))
   if (b != bb) {
     bb <<- b
     tkrreplot(img)
   }
 }
 s <- tkscale(tkf, command=f, from=0.05, to=2.00, variable="bb",
              showvalue=FALSE, resolution=0.05, orient="horiz")
 tkgrid(s)
 tkgrid(img)
 tkgrid(tkf)
 
 ### Message
 
 require(tcltk)
 tt<-tktoplevel()
 Name <- tclVar("Anonymous")
 entry.Name <-tkentry(tt,width="20",textvariable=Name)
 tkgrid(tklabel(tt,text="Please enter your first name."))
 tkgrid(entry.Name)
 OnOK <- function()
 {
   NameVal <- tclvalue(Name)
   tkdestroy(tt)
   msg <- paste("You have a nice name,",NameVal)
   tkmessageBox(message=msg) # Create popup window with message
 }
 OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
 tkbind(entry.Name, "<Return>",OnOK)
 tkgrid(OK.but)
 tkfocus(tt)
 
 
 ### Interactive plot
 
 xCoords<-(-12:13)
 yCoords<-xCoords*xCoords
 labelsVec <- LETTERS
 require(tcltk)
 require(tkrplot)
 indexLabeled<-c()
 labeledPoints <- list()
 tt <- tktoplevel()
 tkwm.title(tt,"Click on a point to label it")
 parPlotSize <- c()
 usrCoords <- c()
 
 plotFunction <- function()
 {
   params <- par(bg="white")
   plot(xCoords,yCoords,main="Click on a point to label it")
   if (length(indexLabeled)>0)
     for (i in (1:length(indexLabeled)))
     {
       indexClosest <- indexLabeled[i]
       text(xCoords[indexClosest],yCoords[indexClosest],
            labels=labelsVec[indexClosest],pos=3)
     }
   parPlotSize <<- par("plt")
   usrCoords   <<- par("usr")
   par(params)
 }
 
 img <- tkrplot(tt,fun=plotFunction,hscale=1.5,vscale=1.5)
 tkgrid(img)
 
 labelClosestPoint <- function(xClick,yClick,imgXcoords,imgYcoords)
 {
   squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
   indexClosest <- which.min(squared.Distance)
   indexLabeled <<- c(indexLabeled,indexClosest)
   tkrreplot(img)
 }
 
 OnLeftClick <- function(x,y)
 {
   xClick <- x
   yClick <- y
   require(tcltk)
   width  <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
   height <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
   
   xMin <- parPlotSize[1] * width
   xMax <- parPlotSize[2] * width
   yMin <- parPlotSize[3] * height
   yMax <- parPlotSize[4] * height
   
   rangeX <- usrCoords[2] - usrCoords[1]
   rangeY <- usrCoords[4] - usrCoords[3]
   
   imgXcoords <- (xCoords-usrCoords[1])*(xMax-xMin)/rangeX + xMin
   imgYcoords <- (yCoords-usrCoords[3])*(yMax-yMin)/rangeY + yMin
   
   xClick <- as.numeric(xClick)+0.5
   yClick <- as.numeric(yClick)+0.5
   yClick <- height - yClick
   
   xPlotCoord <- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
   yPlotCoord <- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)
   
   msg <- paste("Label the point closest to these approximate plot coordinates: \n",
                "x =",format(xPlotCoord,digits=2),",y =",format(yPlotCoord,digits=2),"?")
   mbval<- tkmessageBox(title="Label Point Closest to These Approximate Plot Coordinates",
                        message=msg,type="yesno",icon="question")
   
   if (tclvalue(mbval)=="yes")
     labelClosestPoint(xClick,yClick,imgXcoords,imgYcoords)
 }
 
 tkbind(img, "<Button-1>",OnLeftClick)
 tkconfigure(img,cursor="hand2")
 
 ### Change color using color palette
 
 require(tcltk)
 tt <- tktoplevel()
 tkwm.title(tt,"Color Selection")
 color <- "blue"
 canvas <- tkcanvas(tt,width="80",height="25",bg=color)
 ChangeColor <- function()
 {
   color <- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color,title="Choose a color"))))
   if (nchar(color)>0)
     tkconfigure(canvas,bg=color)
 }
 ChangeColor.button <- tkbutton(tt,text="Change Color",command=ChangeColor)
 tkgrid(canvas,ChangeColor.button)
 
 
 
 
 
# End script
 