# KFAS example from paper
 library(KFAS)
 library(data.table)
 library(ggplot2)
 library(reshape2)
 library(forecast)
 library(MASS)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/KFAS/")
 
 dat <- data.table(read.csv("020_ksyyt_tau_102cm.csv",header=F,stringsAsFactors=F))
 setnames(dat,c("Gender","Age","Cause",1969:2013))
 dat[Gender == "Genders total",Gender:="All"]
 
 dat <- melt(dat,id.vars=c("Gender","Age","Cause"),variable.name="Year",value.name="Deaths",variable.factor=F)
 dat$Year <- as.numeric(dat$Year)
 setkeyv(dat,c("Gender","Age","Cause","Year"))
 
 tabA <- dat[Gender=="All"]
 tab1 <- dat[Gender=="Males"]
 tab2 <- dat[Gender=="Females"]
 identical(tabA[,Deaths],tab1[,Deaths]+tab2[,Deaths])
 
 genders<- c("All","Males","Females")
 years  <- 1969:2013
 causes <- unique(dat$Cause)
 ages   <- unique(dat$Age)
 ages <- ages[order(as.numeric(gsub("([[:alnum:]]*)\\s.*","\\1",ages)))]
 
 
 dat2 <- data.table(read.csv("125_vaerak_tau_106cm.csv",stringsAsFactors=F,header=F))
 setnames(dat2,c("Year",apply(expand.grid(c("Total",0:100),genders)[,2:1],1,paste,collapse=" ")))
 dat2 <- melt(dat2,id.vars="Year",variable.factor=F,value.name="Deaths")
 dat2 <- cbind(dat2[,list(Year)],t(as.data.frame(strsplit(dat2$variable," "))),dat2[,list(Deaths)])
 setnames(dat2,c("V1","V2"),c("Gender","Age"))
 setkeyv(dat2,c("Gender","Age","Year"))
 
 
 
 # Create data used in paper
 newdat <- dat[Age %in% ages[8:15] & Cause ==causes[3] & Gender=="All",list(.N,Deaths=sum(Deaths)),by=list(Age=substr(Age,1,1),Year)]
 newdat <- dat[Age %in% ages[8:15] & grepl("alcohol",Cause,ignore.case=T) & Gender=="All",list(.N,Deaths=sum(Deaths)),by=list(Age=substr(Age,1,1),Year)]
 newdat[,Age:=paste0(Age,"0 - ",Age,"9")]
 newdat
 
 ggplot(newdat,aes(Year,Deaths,col=Age)) + geom_line()
 ggplot(newdat,aes(Year,Deaths/1e5,col=Age)) + geom_line()
 
 # ... tbc
 
 
 
 
 
# End script
 