# Functions for plotting data on maps using plotGoogleMaps, d3 and svg
# Attempt to write functions to use ggplot syntax
 library(data.table)
 library(ggmap)
 library(plotGoogleMaps)
 library(rjson)
 
def_symbols <- "
// DEFINE A COUPLE OF CUSTOM SYMBOLS
var customSymbolTypes = d3.map({
  'star': function(size) {
    // returns a path-data string
    return 'M0 0 H 10 V 10 L 0 0';
  },
  'cross2': function(size) {
    // returns a path-data string
  return 'M0 3.5 L 3.5,7 7,3.5 3.5,0 7,-3.5 3.5,-7 0,-3.5 -3.5,-7 -7,-3.5, -3.5,0 -7,3.5 -3.5,7 0,3.5';
  }
});


// CREATE A CUSTOM SYMBOL FUNCTION MIRRORING THE BUILT-IN FUNCTIONALITY
d3.svg.customSymbol = function() {
  var type,
      size = 64; // SET DEFAULT SIZE
  function symbol(d,i) {
    // GET THE SYMBOL FROM THE CUSTOM MAP
    return customSymbolTypes.get(type.call(this,d,i))(size.call(this,d,i));
  }
  // DEFINE GETTER/SETTER FUNCTIONS FOR SIZE AND TYPE
  symbol.type = function(_) {
    if (!arguments.length) return type;
    type = d3.functor(_);
    return symbol;
  };
  symbol.size = function(_) {
    if (!arguments.length) return size;
    size = d3.functor(_);
    return symbol;
  };
  return symbol;
};"
 
 cgm_init <- function() {
   html <- paste('<meta name="viewport" content="initial-scale=1.0, user-scalable=no"/>',
                 '<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=true"></script>',
                 '<script type="text/javascript" src="http://square.github.io/crossfilter/d3.v3.min.js"></script>',
                 '<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.3.min.js"></script>',
                 '<script>',
                 def_symbols,
                 'var sym_circle = d3.svg.symbol().size(100).type("circle");',
                 'var sym_upTri  = d3.svg.symbol().size(100).type("triangle-up");',
                 'var sym_dnTri  = d3.svg.symbol().size(100).type("triangle-down");',
                 'var sym_cross  = d3.svg.symbol().size(100).type("cross");',
                 'var sym_dimnd  = d3.svg.symbol().size(100).type("diamond");',
                 'var sym_square = d3.svg.symbol().size(100).type("square");',
                 'var sym_star = d3.svg.customSymbol().type("star").size(100);',
                 'var sym_cross2 = d3.svg.customSymbol().type("cross2").size(100);',
                 'var symbols = {"circle":sym_circle,"square":sym_square,"cross":sym_cross,"dimnd":sym_dimnd,"upTri":sym_upTri,"dnTri":sym_dnTri,"star":sym_star,"cross2":sym_cross2};',
                 '</script>',
                 '\n',sep="\n")
   html
 }
 
 cgm_styles <- function(styles) {
   strings <- paste(
     sapply(
       seq_along(styles),
       function(i) {
         if(is.list(styles[[i]])) {
           paste(
             sapply(
               seq_along(styles[[i]]),
               function(j) {
                 paste0(".",names(styles)[i], " ", names(styles[[i]])[[j]], " {\n", paste("  ", names(styles[[i]][[j]]), ": ", styles[[i]][[j]],";", sep = "", collapse = "\n"), "\n}")
               }
             ),
             collapse="\n\n"
           )
         } else {
           paste0(".", names(styles)[i], " {\n", paste("  ", names(styles[[i]]), ": ", styles[[i]], ";",sep = "", collapse = "\n"), "\n}")
         }
       }
     ),
     collapse = "\n\n"
   )
   html <- paste("\n<style>", strings, "</style>\n", sep = "\n")
   html
 }
 
 header <- function(styles = NULL) {
   html <- cgm_init()
   if (!is.null(styles)) {
     htmlstyles <- cgm_styles(styles)
     html <- paste(html, htmlstyles, sep = "\n")
   }
   html
 }
 
 cgm_findbounds <- function(objects) {
   # Find optimal center coordinates and zoom factor (this is not calculated precisely, zoom is not optimal)
   if (all(sapply(objects, function(x) is.null(x$data)))) {
     #print("Null data")
     return(list(minlat=40,maxlat=45,minlon=-100,maxlon=0))
   }
   minlat <- min(sapply(objects, function(obj) min(obj$data$lat)))
   minlon <- min(sapply(objects, function(obj) min(obj$data$lon)))
   maxlat <- max(sapply(objects, function(obj) max(obj$data$lat)))
   maxlon <- max(sapply(objects, function(obj) max(obj$data$lon)))
   
   midlon <- mean(c(minlon, maxlon))
   midlat <- mean(c(minlat, maxlat))
   
   zoom <- min(10.8 - log2(maxlon-minlon), 9.3 - log2(maxlat-minlat))
   
   bounds <- list(midlon = midlon, midlat = midlat, zoom = zoom, minlat = minlat, maxlat = maxlat, minlon = minlon, maxlon = maxlon)
 }
 
 cgm_js <- function(..., jsFUNs = NULL) {
   objects = list(...)
   # Find center coordinates and best zoom factor
   bounds <- cgm_findbounds(objects)
   
   # wrap objects in script tags, initializing the map
   objectsJSON <- toJSON(objects)
   html <- paste("<script>\n",
                 "$(document).ready(function() {\n",
                 "var map;\n",
                 "var overlay;\n",
                 "var layer;\n",
                 "var bounds;\n\n",
                 "var n={};\n",
                 "var g1, g2;\n",
                 "function initialize() {\n",
                 "  options  = {zoom: 4, mapTypeId: google.maps.MapTypeId.TERRAIN }\n",
                 '  map      = new google.maps.Map(d3.select("#map_canvas").node(),options);\n',
                 "  bounds   = new google.maps.LatLngBounds(new google.maps.LatLng(",bounds$minlat,",",bounds$minlon,"), new google.maps.LatLng(",bounds$maxlat,",",bounds$maxlon,"));\n",
                 "  overlay  = new google.maps.OverlayView();\n",
                 "  overlay.onAdd = function() {\n",
                 paste("g",seq_along(objects),' = d3.select(this.getPanes().overlayMouseTarget).append("div").attr("class", "stations");\n',sep="",collapse=""),
                 "  }\n",
                 "}\n\n",
                 "initialize();\n\n",
                 "overlay.draw = function() {\n",
                 
                 paste(sapply(seq_along(objects),function(i) {
                   paste('var data = ',objects[[i]]$json,';\n',
                         'var projection = this.getProjection(), padding = 30;\n',
                         'var marker',i,' = g',i,'.selectAll("svg").data(d3.entries(data)).each(transform).enter().append("svg:svg").each(transform).attr("class", "marker");\n\n',
                         '\n  marker',i,objects[[i]]$d3js,
                         '\n  marker',i,'.append("svg:text").attr("x", padding + 7).attr("y", padding).attr("dy", ".31em").attr("class","marker_text").text(function(d) {return d.key; });\n\n'
                         ,sep="")
                 }),collapse="\n\n"),
                 
                 ifelse(is.null(jsFUNs),"",paste("function ",names(jsFUNs),jsFUNs,";\n\n",sep="",collapse="") ),
                 
                 'function transform(d) {\n',
                 '  d = new google.maps.LatLng(d.value.lat, d.value.lon);\n',
                 '  d = projection.fromLatLngToDivPixel(d);\n',
                 '  return d3.select(this).style("left", (d.x - padding) + "px").style("top", (d.y - padding) + "px");\n}\n\n',
                 
                 "};\n",
                 "overlay.setMap(map);\n",
                 "\n\n",
                 'map.fitBounds(bounds);\n',
                 '});\n',
                 "</script>"
                 ,sep="")
   html
 }
 
 map <- function(lon, lat, ...) {
   map <- structure(as.list(match.call()[-1]), class = "uneval")
   map
 }
 
 #**
 defaults <- data.table(R = c("col", "fill", "stroke", "size", "group", "alpha", "strokealpha", "fillalpha", "lon", "lat"),
                        #SVG=c("stroke","fill","stroke_width","scale","symbol","opacity","stroke_opacity","fill_opacity","lon","lat"),
                        Default = c("black", "red", 1, 1, "circle", 1, 1, 1, NA, NA),
                        Call = c("style", "style", "style", "attr", "symbol", "style", "style", "style", "attr", "attr"))
 defaults <- list(col = "black",
                  fill = "red",
                  stroke = 1,
                  size = 1,
                  group = "circle",
                  alpha = 1,
                  strokealpha = 1,
                  fillalpha = 1,
                  lon = NA,
                  lat = NA)
 
 get_value <- function(x, range, default = 1, log = F) {
   tmp <- x
   ind <- which(!is.na(tmp))
   if(length(unique(tmp[ind])) > 1) {
     if(mode(tmp)!="numeric")
       tmp[ind] <- as.numeric(as.factor(tmp[ind]))
     if (log)
       tmp <- log(tmp)
     tmp[ind] <- (tmp[ind]-min(tmp[ind]))/(max(tmp[ind])-min(tmp[ind]))*diff(range)+range[1]
   } else {
     if(mode(tmp[ind])!="numeric")
       tmp[ind] <- default
   }
   as.numeric(tmp)
 }
 get_color <- function(x, pal, default = "black", log = F) {
   #pal <- opts$col_pal
   tmp <- x
   ind <- which(!is.na(tmp))
   if (length(unique(tmp[ind])) > 1) {
     if (class(tmp) == "factor") {
       tmp[ind] <- pal[as.numeric(tmp[ind])]
     } else {
       if (log) {
         tmp <- log(tmp)
         ind <- which(!is.na(log(tmp)))
       }
       tmp[ind] <- pal[(tmp[ind]-min(tmp[ind]))/(max(tmp[ind])-min(tmp[ind]))*(length(pal)-1) + 1]
     }
   } else {
     if (!is.color(unique(tmp[ind])))
       tmp[ind] <- default
   }
   tmp
 }
 
 add_point <- function(data, mapping, opts = list()) {
   # mapping should be created using map() function.
   
   #data <- dat1
   data <- as.data.table(data)
   
   # translate names in r to svg names ??
   #mapping <- map(lon=lon,lat=lat,fill="blue")
   #mapping <- map(lon=long,lat=lat,fill="green",size=Scale,group=letters[Stroke],col="red")
   
   #mapping[defaults[! R %in% names(mapping),R]] <- defaults[! R %in% names(mapping),Default]
   #mapping <- mapping[match(defaults$R,names(mapping))]
   
   mapping <- merge.list(mapping,defaults)
   #names(mapping) <- defaults$SVG
   
   dt <- data[,lapply(mapping,eval,envir=data)]
   
   # opts controls the range of plotting values, color schemes, etc.
   # opts <- list()
   opts <- merge.list(opts,list(size_range   = c(1,2), 
                                stroke_range = c(1,4), 
                                alpha_range  = c(.3,1),
                                symbols      = c("circle", "upTri", "square", "dnTri", "cross", "dimnd", "star", "cross2"),
                                col_palette  = switch(class(dt$col),
                                                      integer   = ,
                                                      numeric   = colorRampPalette(c("blue", "white", "red"))(50),
                                                      factor    = lighten(rainbow(nlevels(dt$col)))),
                                fill_palette = switch(class(dt$fill),
                                                      integer   = ,
                                                      numeric   = colorRampPalette(c("blue", "white", "red"))(50),
                                                      factor    = lighten(rainbow(nlevels(dt$fill)))),
                                size_log   = F,
                                stroke_log = F,
                                strokealpha_log = F,
                                fillalpha_log   = F,
                                col_log  = F,
                                fill_log = F
   ))
   
   if(is.null(opts$fillalpha_range))
     opts$fillalpha_range <- opts$alpha_range
   if(is.null(opts$strokealpha_range))
     opts$strokealpha_range <- opts$alpha_range
   
   if(is.null(dt$strokealpha))
     dt$strokealpha <- dt$alpha
   if(is.null(data$fillalpha))
     dt$fillalpha <- dt$alpha
   
   if(class(dt$col)  == "character")
     dt$col  <- factor(dt$col)
   if(class(dt$fill) == "character")
     dt$fill <- factor(dt$fill)
   
   dt$size        <- get_value (dt$size,        opts$size_range,        defaults$size,        opts$size_log)
   dt$stroke      <- get_value (dt$stroke,      opts$stroke_range,      defaults$stroke,      opts$stroke_log)
   dt$strokealpha <- get_value (dt$strokealpha, opts$strokealpha_range, defaults$strokealpha, opts$strokealpha_log)
   dt$fillalpha   <- get_value (dt$fillalpha,   opts$fillalpha_range,   defaults$fillalpha,   opts$fillalpha_log)
   dt$col         <- get_color (dt$col,         opts$col_palette,       defaults$col,         opts$col_log)
   dt$fill        <- get_color (dt$fill,        opts$fill_palette,      defaults$fill,        opts$fill_log)
   
   dt[!is.na(group),group:={
     tmp <- ((as.numeric(as.factor(group)) - 1) %% length(opts$symbols)) + 1
     #if(length(unique(tmp))>1)
     #tmp <- symbols[]
     tmp <- opts$symbols[tmp]
     tmp
   }]
   
   dt <- dt[order(size,decreasing=T)]
   
   #d3js <- ".append('svg:circle').attr('cx', padding).attr('cy', padding).attr('r', function(d) {return d.value.scale;}).style('stroke',function(d) {return d.value.stroke}).style('stroke-width',function(d) {return d.value.stroke_width}).style('fill',function(d) {return d.value.fill}).on('click',expandNode).on('dblclick',null).on('mouseover',function(d){ console.log(d.key); })"
   #d3js <- ".append('svg:circle').attr('cx', padding).attr('cy', padding).attr('r', function(d) {return d.value.size;}).style('stroke',function(d) {return d.value.col}).style('stroke-width',function(d) {return d.value.stroke}).style('fill',function(d) {return d.value.fill}).on('click',expandNode).on('dblclick',null).on('mouseover',function(d){ console.log(d.key); })"
   
   d3js <- paste(".append('path')",
                 ".attr('d',function(d) {console.log(d); return symbols[d.value.group]();})",
                 ".style('stroke',function(d) {return d.value.col;})",
                 ".style('stroke-width',function(d) {return d.value.stroke;})",
                 ".style('fill',function(d) {return d.value.fill;})",
                 ".style('stroke-opacity',function(d) {return d.value.strokealpha})",
                 ".style('fill-opacity',function(d) {return d.value.fillalpha})",
                 ".attr('transform',function(d) {return 'translate('+padding+','+padding+') scale('+d.value.size+')' ;})",
                 ".on('mouseover',function(d){ console.log(1); })" )
   
   json <- paste("[",paste(sapply(1:nrow(dt),function(i) toJSON(dt[i])),collapse=","),"]",sep="")
   
   obj <- list(type="point",data=dt,json=json,d3js=d3js)
   obj
 }
 
 
 #objects <- list(data=list(London=list(-0.1198244,51.5112139,"Capital of Great Britain"),WashingtonDC=list(-77.0364641,38.9072309,"Capital of United States")),type="circle")
 #toJSON(list(objects))
 
 #dat <- data.table(lon=c(-0.1198244,-77.0364641),lat=c(51.5112139,38.9072309),city=c("London","WashingtonDC"),description=c("Capital of Great Britain","Capital of United States"))
 #add_point(dat,map(lon,lat))
 
 
 
 #lst <- list(stations=list(svg=list(width="60px",height="20px"),circle=list(fill="red",stroke="black"),"circle:hover"=list(fill="blue")),marker_text=list(fill="black",visibility="hidden"))
 
 
 
 
 
# End script
 