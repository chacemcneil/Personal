## Basic code for creating distance map
 library(CMcCode)
 library(data.table)
 library(geosphere)
 library(gtools)
 library(leaflet)
 library(maps)
 library(scales)
 
 rainbowcols <- c("red", "orange", "gold", "springgreen2", "deepskyblue", "blue", "purple4", "black")
 
 dist_map <- function(dat, cols = rainbowcols, centerpt = NULL, by = c("quantile", "range")) {
   ## Data entered as data.frame object containing the following parameters:
   ##  long:    longitude of point
   ##  lat:     latitude of point
   ##  dist:    distance from a central point (if missing, a central point is calculated)
   
   require(data.table)
   dat <- data.table(dat)
   cols <- hex(cols)
   numcols <- length(cols)
   if(is.character(by))
     by <- match.arg(by)
   
   if(is.null(dat$dist)) {
     if(is.null(centerpt)) {
       centerlong <- mean(dat$long)
       centerlat <- mean(dat$lat)
     } else {
       if ("long" %in% names(centerpt)) {
         centerlong <- centerpt$long
         centerlat <- centerpt$lat
       } else {
         centerlong <- centerpt[1]
         centerlat <- centerpt[2]
       }
     }
     dat[, dist := distGeo(cbind(long, lat), cbind(centerlong, centerlat)) *3.28/5280] # Convert to miles
   }
   
   if(by == "quantile") {
     dat[, distgroup := quantcut(dist, q = (0:numcols)/numcols)]
   } else if(by == "range") {{}
     dat[, distgroup := cut(dist, seq(0, max(dist), length.out = numcols + 1), include.lowest = T)]
   } else {
     dat[, distgroup := cut(dist, c(0, by), include.lowest = T)]
   }
   # pal <- colorNumeric(palette = cols, domain = as.character(dat$distgroup))
   pal <- colorFactor(palette = cols, domain = dat$distgroup)
   dat[, color := cols[distgroup]]
   
   leaflet(dat, options = list(zoomControl = FALSE)) %>%
     setView(lng=mean(range(dat$long)), lat=mean(range(dat$lat)), zoom = 4) %>% 
     addTiles() %>%
     addCircleMarkers(lng=dat$long, lat=dat$lat, color = dat$color, weight = 2, opacity = .8, radius = 8) %>%
     addCircleMarkers(lng=dat$long, lat=dat$lat, color = dat$color, weight = 1, opacity = 1, radius = 1) %>%
     addCircleMarkers(lng=centerlong, lat=centerlat, color = "#555555", weight = 2, opacity = 1, radius = 2) %>%
     addLegend("bottomright", pal = pal, values = ~distgroup)
   
 }
 
 city <- data.table(maps::us.cities)[capital == 2]
 dist_map(city)
 
 
 ## Add tooltip
 leaflet(dat, options = list(zoomControl = FALSE)) %>%
   setView(lng=mean(range(dat$long)), lat=mean(range(dat$lat)), zoom = 4) %>% 
   addTiles() %>%
   # addCircleMarkers(lng=~long, lat=~lat, color = dat$color, weight = 2, opacity = .8, radius = 8, label = htmltools::HTML(hoverText2)) %>%
   # addCircleMarkers(lng=~long, lat=~lat, color = dat$color, weight = 2, opacity = .8, radius = 8,
   #                  label = ~lapply(paste0("<p><span>Capital: ", dat$name, ",</span><br><span>Population: ", comma(dat$pop), "</span></p>"), htmltools::HTML),
   #                  labelOptions = labelOptions(Style = list("white-space" = "pre"))) %>%
   addCircleMarkers(lng=~long, lat=~lat, color = dat$color, weight = 2, opacity = .8, radius = 8,
                    label = ~paste0("<p><span>Capital: ", dat$name, ",</span><br><span>Population: ", comma(dat$pop), "</span></p>"),
                    labelOptions = labelOptions(Style = list("white-space" = "pre"), interactive = T) ) %>%
   addCircleMarkers(lng=dat$long, lat=dat$lat, color = dat$color, weight = 1, opacity = 1, radius = 1) %>%
   addCircleMarkers(lng=centerlong, lat=centerlat, color = "#555555", weight = 2, opacity = 1, radius = 2) %>%
   addLegend("bottomright", pal = pal, values = ~distgroup, title = "Miles from center")
 
 
 
# End script
 