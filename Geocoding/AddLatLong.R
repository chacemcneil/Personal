# R code for adding lat/lon from street address
 library(data.table)
 library(savvy)
 library(ggmap)
 library(RODBC)
 
 # Function to add lat/long
 
 formatAddress <- function(addresses,stpwrds=NULL,stpregex=NULL) {
   addresses <- toupper(addresses)
   
   # Change direction letters to words
   addresses <- gsub("\\bN\\b","NORTH",addresses)
   addresses <- gsub("\\bS\\b","SOUTH",addresses)
   addresses <- gsub("\\bE\\b","EAST",addresses)
   addresses <- gsub("\\bW\\b","WEST",addresses)
   
   # Remove given stop words (e.g. "suite") and regex
   stpwrds <- paste0("(",paste("\\b",toupper(stpwrds),"\\b",sep="",collapse="|"),")")
   addresses <- gsub(stpwrds,"",addresses)
   addresses <- gsub(stpregex,"",addresses)
   
   # Remove unnecessary white space
   addresses <- gsub("(^\\s*|\\s*$)","",addresses)
   addresses <- gsub("\\s+"," ",addresses)
   
   addresses
 }
 
 addLatLong <- function(dat,address="Address") {
   require(ggmap)
   if(! "data.table" %in% class(dat))
     dat <- data.table(dat)
   unq.add <- unique(dat[,address,with=F])                       # Find unique addresses (for efficiency)
   unq.add <- formatAddress(unq.add,                             # Format street addresses
                            stpregex="(\\bSTE\\b|\\bSUITE\\b|#)(\\s*)?[0987654321]*")
   setkeyv(unq.add,address)                                      # Set Address as key value
   
   unq.add <- cbind(unq.add,geocode(unq.add[[address]]))         # Use geocode() to get lat/lon coordinates (still slow)
   
   setkeyv(dat,address)                                          # Set Address as key value for merge
   dat <- merge(dat,unq.add,all.x=T)                             # Join lat/lon to data
   dat
 }
 
 #addLatLong(data.table(Address="6500 Shingle Creek Pkwy"))
 
 #read.odbc("D10pdb_CRAEngine",dbQuery="select * from information_schema.tables")
 
 addresses <- c("45529jilsk 334 jksl n ste #34","234j9fldas reum dfsa9f3 e suite # 35 tx")
 stpwrds <- c("ste","suite")
 stpregex <- "#(\\s*)?[0987654321]*"
 formatAddress(addresses,stpwrds=stpwrds,stpregex=stpregex)
 
 
 
 
 
# End script
 