# Coordinates of 2017 eclipse
library(data.table)
library(dplyr)
library(leaflet)

setwd(gsub("/[^/]*$", "", rstudioapi::getActiveDocumentContext()$path))

coords <- data.table(read.table("eclipse_coords.txt"))
setnames(coords, c("Time", "N_Lat_deg", "N_Lat_part", "N_Long_deg", "N_Long_part",
                   "S_Lat_deg", "S_Lat_part", "S_Long_deg", "S_Long_part",
                   "C_Lat_deg", "C_Lat_part", "C_Long_deg", "C_Long_part",
                   "Diam_Ratio", "Sun_Alt", "Sun_Azm", "Path_width", "Central_Line_dur"))

coords[, N_Lat := (2 * grepl("N", N_Lat_part) - 1) * (N_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", N_Lat_part))/60)]
coords[, S_Lat := (2 * grepl("N", S_Lat_part) - 1) * (S_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", S_Lat_part))/60)]
coords[, C_Lat := (2 * grepl("N", C_Lat_part) - 1) * (C_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", C_Lat_part))/60)]
coords[, N_Long := (2 * grepl("E", N_Long_part) - 1) * (N_Long_deg + as.numeric(gsub("(N|S|E|W)", "", N_Long_part))/60)]
coords[, S_Long := (2 * grepl("E", S_Long_part) - 1) * (S_Long_deg + as.numeric(gsub("(N|S|E|W)", "", S_Long_part))/60)]
coords[, C_Long := (2 * grepl("E", C_Long_part) - 1) * (C_Long_deg + as.numeric(gsub("(N|S|E|W)", "", C_Long_part))/60)]

coords2 <- data.table(read.table("eclipse_coords2.txt"))
setnames(coords2, c("Time", "N_Lat_deg", "N_Lat_part", "N_Long_deg", "N_Long_part",
                    "S_Lat_deg", "S_Lat_part", "S_Long_deg", "S_Long_part",
                    "C_Lat_deg", "C_Lat_part", "C_Long_deg", "C_Long_part",
                    "Diam_Ratio", "Sun_Alt", "Sun_Azm", "Path_width", "Central_Line_dur"))

coords2[, N_Lat := (2 * grepl("N", N_Lat_part) - 1) * (N_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", N_Lat_part))/60)]
coords2[, S_Lat := (2 * grepl("N", S_Lat_part) - 1) * (S_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", S_Lat_part))/60)]
coords2[, C_Lat := (2 * grepl("N", C_Lat_part) - 1) * (C_Lat_deg + as.numeric(gsub("(N|S|E|W)", "", C_Lat_part))/60)]
coords2[, N_Long := (2 * grepl("E", N_Long_part) - 1) * (N_Long_deg + as.numeric(gsub("(N|S|E|W)", "", N_Long_part))/60)]
coords2[, S_Long := (2 * grepl("E", S_Long_part) - 1) * (S_Long_deg + as.numeric(gsub("(N|S|E|W)", "", S_Long_part))/60)]
coords2[, C_Long := (2 * grepl("E", C_Long_part) - 1) * (C_Long_deg + as.numeric(gsub("(N|S|E|W)", "", C_Long_part))/60)]




cities <- data.table(city = paste("Salem,", c("OR", "ID", "NE", "MO", "KY", "SC", "TN", "IL", "NC", "GA", "SD", "AL",
                                              "MI", "UT", "OK", "KS", "NM", "MT", "TX", "AK", "MA", "IN", "OH", "VA",
                                              "WI", "NY", "NJ", "CT", "NH", "WV", "MN", "IO", "FL", "MS", "ME") ),
                     lat = c(44.94, 43.8766, 40.0756, 37.6456, 37.2645, 34.8898, 35.8092, 38.6270, 35.6987, 32.7587, 43.7241, 32.5968,
                             42.3929, 40.0530, 35.7665, 37.5228, 32.7127, 47.5372, 31.7602, 36.3712, 42.5195, 38.6056, 40.9009, 37.2935,
                             42.5542, 43.1723, 39.5718, 41.4904, 42.7886, 39.2829, 43.9714, 40.8528, 29.8869, 31.2299, 44.9053),
                     long = c(-123.0351, -111.7730, -95.7208, -91.536, -88.2442, -82.9765, -86.4772, -88.9456, -81.6970, -84.1877, -97.3890, -85.2386,
                              -83.6136, -111.6735, -94.5730, -97.3324, -107.2046, -111.0402, -95.1386, -91.8226, -70.8967, -86.1011, -80.8568, -80.0548,
                              -88.1105, -73.3276, -75.4671, -72.2754, -71.2009, -80.5590, -92.5991, -91.6202, -83.4129, -90.1173, -70.2644),
                     group  = factor(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), labels = c("Main", "Other")),
                     iscity = factor(c(1, 2, 2, 1, 1, 2, 4, 1, 4, 4, 1, 1, 3, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 1, 3, 1, 4, 4, 4), labels = c("City", "Town", "Township", "Area")) )


leaflet(options = list(zoomControl = FALSE)) %>%
  setView(lng=sum(range(coords$C_Long))/2, lat=sum(range(coords$C_Lat))/2, zoom = 4) %>% 
  addTiles() %>%
  # addAwesomeMarkers(lng=-coords$C_Long_deg, lat=coords$C_Lat_deg, popup=coords$Time ) %>%
  addAwesomeMarkers(lng=cities$long, lat=cities$lat, popup=cities$city, group = cities$group,
                    icon = awesomeIcons(icon = "ios-close", iconColor = "black", library = "ion",
                                        markerColor = c("blue", "lightgreen", "lightblue", "orange")[cities$iscity] ) ) %>%
  addPolylines(lng=coords$C_Long, lat=coords$C_Lat, popup=coords$Time, weight = 2 ) %>%
  addPolylines(lng=coords$N_Long, lat=coords$N_Lat, popup=coords$Time, weight = .5 ) %>%
  addPolylines(lng=coords$S_Long, lat=coords$S_Lat, popup=coords$Time, weight = .5 ) %>%
  addPolylines(lng=coords2$C_Long, lat=coords2$C_Lat, popup=coords2$Time, weight = 2, col = "orange" ) %>%
  addPolylines(lng=coords2$N_Long, lat=coords2$N_Lat, popup=coords2$Time, weight = .5, col = "orange" ) %>%
  addPolylines(lng=coords2$S_Long, lat=coords2$S_Lat, popup=coords2$Time, weight = .5, col = "orange" ) %>%
  addLayersControl(overlayGroups = c("Main", "Other"), options = layersControlOptions(collapsed = FALSE)) %>% addSearchOSM() %>% addDrawToolbar()


# End script