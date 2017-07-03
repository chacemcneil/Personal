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
 
 addLatLong <- function(dat,address="Address",latlon=NULL) {
   dat <- dat[,.SD]
   setnames(dat,address,"Address")
   if(!is.null(latlon)) {
     # Only retrieve lat/lon for places not already included (save unnecessary queries)
     setnames(dat,latlon,c("lat","lon"))
     comp <- dat[!is.na(lat)]
     empt <- dat[is.na(lat)][,lat:=NULL][,lon:=NULL]
     dat <- rbind(comp,addLatLong(empt))
   }
   else {
     require(ggmap)
     if(! "data.table" %in% class(dat))
       dat <- data.table(dat)
     dat$Address <- formatAddress(dat$Address,                             # Format street addresses
                                     stpregex="(\\bSTE\\b|\\bSUITE\\b|#)(\\s*)?[[:alnum:] ]*")
     
     unq.add <- unique(dat[,address,with=F])                       # Find unique addresses (for efficiency)
     setkeyv(unq.add,address)                                      # Set Address as key value
     
     unq.add <- cbind(unq.add,geocode(unq.add$Address))            # Use geocode() to get lat/lon coordinates (still slow)
     
     setkeyv(dat,"Address")                                        # Set Address as key value for merge
     dat <- merge(dat,unq.add,all.x=T)                             # Join lat/lon to data
   }
   dat
 }
 
 geoDistance <- function(lat1,lon1,lat2,lon2,degreeinputs=T,method=c("equirectangular","cosines","haversine")) {
   method <- match.arg(method)
   RadEarth <- 6.371e6  # Radius of Earth in Meters
   if(degreeinputs) {
     lat1rad <- pi/180*lat1
     lat2rad <- pi/180*lat2
     lon1rad <- pi/180*lon1
     lon2rad <- pi/180*lon2
   }
   
   if(method == "equirectangular") {
     latmid <- (lat1rad + lat2rad)/2
     x = (lon2rad-lon1rad)*cos(latmid)
     y = lat2rad-lat1rad
     d = RadEarth*sqrt(x^2+y^2)      # distance in meters
   }
   else if(method == "cosines") {
     d <- RadEarth*acos(sin(lat1rad)*sin(lat2rad)+cos(lat1rad)*cos(lat2rad)*cos(lon2rad-lon1rad))
   }
   else if(method == "haversine") {
     a = sin((lat2rad-lat1rad)/2)^2 + cos(lat1rad)*cos(lat2rad)*sin((lon2rad-lon1rad)/2)^2
     c = 2*atan2(sqrt(a),sqrt(1-a))
     d = RadEarth*c                     # distance in meters
   }
   d/1600         # in miles
 }
 # geoDistance(40.2989,-111.7193,40.49927,-111.8907)
 
 #addLatLong(data.table(Address="6500 Shingle Creek Pkwy"))
 
 #read.odbc("D10pdb_CRAEngine",dbQuery="select * from information_schema.tables")
 
 addresses <- c("45529jilsk 334 jksl n ste #34","234j9fldas reum dfsa9f3 e suite # 35 tx")
 stpwrds <- c("ste","suite")
 stpregex <- "#(\\s*)?[0987654321]*"
 formatAddress(addresses,stpwrds=stpwrds,stpregex=stpregex)
 
 
 
 
 
# End script
 