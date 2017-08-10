# Working with maps
 library(data.table)
 library(ggmap)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(maptools)
 library(plotGoogleMaps)
 library(rgdal)
 library(rgeos)
 
 
 
 #download.file("http://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e5","../Miscellaneous/Personal/usmapdata.zip",method="curl")
 #unzip("../../Miscellaneous/Personal/usmapdata.zip")
 
 all_states_shp <- readOGR("/work/cmcneil/Miscellaneous/Personal/states_21basic", "states",verbose=F)
 dfstates <- as.data.table(as.data.frame(all_states_shp))
 setnames(dfstates,c("State","Seq","FIPS","SubRegion","Abbr"))
 
 all_states <- data.table(fortify(all_states_shp,region="STATE_NAME"))
 all_states <- merge(dfstates[,list(State,SubRegion,Abbr)],
                     all_states[,list(State=id,long,lat,order,hole,piece,group)],
                     by="State")
 ak <- subset(all_states, State=="Alaska")
 hi <- subset(all_states, State=="Hawaii")
 states <- subset(all_states, ! State %in% c("Alaska","Hawaii"))
 
 states
 
 ggusa <- ggmap(get_map("USA",zoom=3)) + scale_x_continuous(limits=c(-125,-65)) + scale_y_continuous(limits=c(25,50)) + labs(x="",y="")
 
 #ggplot(all_states,aes(long,lat,group=group,fill=id)) + geom_polygon(col="black")
 
 #states <- map_data("state")
 #states50 <- rbind(states,map_data("usa"))
 
  
 
 
 
 if(F) {
   
   plotGoogle <- function (df,lon="long",lat="lat",file="Map1.html",col=NULL,size=NULL,colPalette=c("orange","yellow","white"),...) {
     if(is.null(file)) {
       num <- 1
       while (paste0("Map",num) %in% dir()) {
         num <- num + 1
       }
       file <- paste0("Map",num,".html")
     } else {
       if (!grepl(".html$",file))
         file <- paste0(file,".html")
     }
     
     df <- as.data.frame(df)
     coordinates(df) <- as.formula(paste("~",lon,"+",lat))
     proj4string(df) <- CRS('+proj=longlat')
     
     plotGoogleMaps(df,
                    filename="Projection.html",
                    zcol=col,
                    mapTypeId="TERRAIN",
                    legend=F,
                    fitBounds=F,
                    zoom = 7,
                    colPalette=colPalette,
                    streetViewControl=T,
                    openMap=F,...)
     invisible(1)
   }
   
   bubbleGoogle <- function (df,lon="long",lat="lat",col=NULL,size=NULL,colPalette=c("orange","yellow","white"),...) {
     df <- as.data.frame(df)
     coordinates(df) <- as.formula(paste("~",lon,"+",lat))
     proj4string(df) <- CRS('+proj=longlat')
     
     col <- ifelse(is.null(col),1,col)
     
     bubbleGoogleMaps(df,
                      filename="Projection.html",
                      zcol=col,
                      mapTypeId="TERRAIN",
                      legend=F,
                      fitBounds=F,
                      zoom = 7,
                      colPalette=colPalette,
                      streetViewControl=T,
                      openMap=F,...)
     invisible(1)
   }
   
   ggmap(get_map("United States",zoom=3)) + geom_polygon(data=states,aes(long,lat,group=group,fill=id),alpha=.3) + scale_x_continuous(limits=c(-125,-65)) + scale_y_continuous(limits=c(25,50)) + labs(x="",y="") + guides(fill=F)
   ggmap(get_map("United States",zoom=3)) + scale_x_continuous(limits=c(-125,-65)) + scale_y_continuous(limits=c(25,50)) + labs(x="",y="") + guides(fill=F)
   
   ggmap(get_map("United States",zoom=3,scale=2))
   
   
   df <- data.table(one=1:10,two=11:20,lon=states$lon[1:10],lat=states$lat[1:10])
   coordinates(df) <- ~lon+lat
   proj4string(df) <- CRS('+proj=longlat')
   
   bubbleGoogleMaps(df, legend=F, control = F,
                    streetViewControl=T,
                    mapTypeId="TERRAIN",
                    api="https://maps.google.com/maps/api/js?sensor=false&v=4")
   
   
   plotGoogle(states[1:3])
   bubbleGoogle(states[1:3],col="order")
   
   
   addAttrToGeom(all)
   
   
   all_states_shp$Value <- rnorm(51)
   plotGoogleMaps(all_states_shp,zcol="Value",mapTypeId="TERRAIN" )
   
   ###### OTHER TUTORIALS
   
   
   
   dir("../../Zipcodes/cb_2014_us_zcta510_500k")
   
   
   G <- plotGoogleMaps(all_states_shp)
   
   names(G)
   write(paste(G,collapse="\n"))
 }
 
 
 
# End script
 