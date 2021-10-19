# Function for writing javascript to create a force D3 graph.
 library(rjson)
 library(igraph)
 
 
 d3init <- function() 
   cat('<meta charset="utf-8"><script src="http://d3js.org/d3.v3.min.js"></script>')
 
 FUNs <- list("+"=function(lst) paste("(",lst[[1]],"+",lst[[2]],")"),
              "-"=function(lst) ifelse(length(lst)==1, paste("(-",lst[[1]],")"),paste("(",lst[[1]],"-",lst[[2]],")")),
              "*"=function(lst) paste("(",lst[[1]],"*",lst[[2]],")"),
              "/"=function(lst) paste("(",lst[[1]],"/",lst[[2]],")"),
              "^"=function(lst) paste("Math.pow(",lst[[1]],",",lst[[2]],")"),
              "sqrt"=function(lst) paste("Math.sqrt(",lst[[1]],")"),
              "log"=function(lst) paste("Math.log(",lst[[1]],")"),
              "log10"=function(lst) paste("Math.log10(",lst[[1]],")"),
              "("=function(lst) paste("(",lst[[1]],")"))
 makeD3function <- function(expr,colclasses="char") {
   jsexpr <- recurseJS(expr[[2]],colclasses)
   str <- paste("function(d) { return (",jsexpr,"); }")
   return(str)
 }
 recurseJS <- function(expr,colclasses="char") {
   #Iterate through call_tree
   if(length(expr)==0) return("")
   if(class(expr)=="name")
       return(paste0("d.",expr))
   op <- expr[[1]]
   if(class(op)=="character") return(paste0("\"",op,"\""))
   if(mode(op)=="numeric") return(op)
   if(class(op) %in% c("name","call")) {
     if(as.character(op) %in% names(FUNs))
       return(FUNs[[as.character(op)]](lapply(expr[-1],recurseJS)))
   }  
 }
 
 myFormat <- function(lst,colclasses="char") {
   lst <- lapply(lst,function(x) {
     if (class(x)=="character")
       return(paste0("\"",x,"\""))
     if (class(x)=="numeric")
       return(x)
     if (class(x)%in%c("formula","call","name"))
       return(makeD3function(x,colclasses))
   })
   return(lst)
 }
 
 myForceID <- function (graph,width=1100,height=900,nodetextbox=NULL,linktextbox=NULL,nodedetails=NULL,linkdetails=NULL,
                        nodes=NULL,links=NULL,duration=300,offset=12,id=ceiling(runif(1)*1000)) {
   ## Plot an igraph object with a D3 force interactive graph.
   
   # nodetextbox=NULL;linktextbox=NULL;nodedetails=NULL;linkdetails=NULL;nodes=NULL;links=NULL
   
   # Load data
   if(class(graph)=="igraph") {
     nodes.df <- as.data.frame(vertex.attributes(graph)) 
     links.df <- as.data.frame(edge.attributes(graph))   # must include 'target' and 'source' fields corresp. to row numbers of nodes.df
   }
   else {
     nodes.df <- graph$v
     links.df <- graph$e
   }
   
   # Set default nodetextbox values
   nodetextboxdef <- list(fillopacity=0.6,strokeopacity=0.7,fill="lightblue",stroke="gray",strokewidth=2,rx=3,ry=3,
                          margin=15,linespacing=20,textitemcol="black",textvaluecol="black")
   nodetextboxdef[names(nodetextbox)] <- nodetextbox
   ntb <- nodetextboxdef
   # Set default linktextbox values
   linktextboxdef <- list(fillopacity=0.6,strokeopacity=0.7,fill="green",stroke="gray",strokewidth=2,rx=3,ry=3,
                          margin=15,linespacing=20,textitemcol="black",textvaluecol="black")
   linktextboxdef[names(linktextbox)] <- linktextbox
   ltb <- linktextboxdef
   
   # Format node data to be displayed in textbox
   nodedetailsdef <- list(items=colnames(nodes.df)[!colnames(nodes.df)%in%c("index")],expr=NULL)
   nodedetailsdef[names(nodedetails)] <- nodedetails
   ndet <- nodedetailsdef
   if(is.null(ndet$expr))
     ndet$expr <- sapply(ndet$items,function(x) as.formula(paste("~",x)))
   ndet$items <- paste0(ndet$items,": ")
   ndet$expr  <- sapply(ndet$expr,function(x) recurseJS(x[[2]]))
   ndet$maxitemlength <- max(nchar(ndet$items))
   ndet$maxexprlength <- max(nchar(sapply(nodes.df,as.character))) + 
                         max(nchar(gsub("(d.[[:alnum:]_]*|Math.[[:alnum:]]*|\\s|\\(\\))","",ndet$expr)))
   
   # Format link data to be displayed in textbox
   linkdetailsdef <- list(items=colnames(links.df)[!colnames(links.df)%in%c("source","target")],expr=NULL)
   linkdetailsdef[names(linkdetails)] <- linkdetails
   ldet <- linkdetailsdef
   if(is.null(ldet$expr))
     ldet$expr <- sapply(ldet$items,function(x) as.formula(paste("~",x)))
   ldet$items <- format(paste0(ldet$items,": "))
   ldet$expr  <- sapply(ldet$expr,function(x) recurseJS(x[[2]]))
   ldet$maxitemlength <- max(nchar(ldet$items))
   ldet$maxexprlength <- max(nchar(sapply(links.df,as.character))) + 
                         max(nchar(gsub("(d.[[:alnum:]_]*|Math.[[:alnum:]]*|\\s|\\(\\))","",ldet$expr)))
   
   # Set default node values
   nodesdef <- list(fill="blue",fillHL="lightblue",stroke="white",strokeHL="red",strokewidth=1.5,strokewidthHL=2,
                    radius=10,radiusHL=20,charge=-120)
   nodesdef[names(nodes)] <- nodes
   nodes <- nodesdef
   # Set default link values
   linksdef <- list(stroke="gray",strokeHL="red",strokewidth=5,strokewidthHL=6,length=80) 
   linksdef[names(links)] <- links
   links <- linksdef
   
   # Format objects
   nodes <- myFormat(nodes,sapply(nodes.df,class))
   links <- myFormat(links,sapply(links.df,class))
   ntb   <- myFormat(ntb)
   ltb   <- myFormat(ltb)
   
   # Other variables
   charwidth <- 7
   if(length(offset)==1) offset <- rep(offset,2)
   
   lst <- list(nodes = lapply(1:nrow(nodes.df),function(i) nodes.df[i,]),
               links = lapply(1:nrow(links.df),function(i) links.df[i,]) )
   
   graphdata <- paste0("graph",id,"=\n",gsub("},","},\n",toJSON(lst)),"\n;")
   
   # Javascript
   html <- paste0('
<script>
var width',id,' = ',width,',
height',id,' = ',height,';
var color',id,' = d3.scale.category20();
var force',id,' = d3.layout.force()
                           .charge(',nodes$charge,')
                           .linkDistance(',links$length,')
                           .size([width',id,', height',id,']);
var svg',id,' = d3.select("body").append("svg")
                                 .attr("width", width',id,')
                                 .attr("height", height',id,')
                                 .on("mousedown",mouseclick',id,');
',graphdata,'
force',id,'.nodes(graph',id,'.nodes)
          .links(graph',id,'.links)
          .start();
var link',id,' = svg',id,'.selectAll(".link")
                          .data(graph',id,'.links)
                          .enter().append("line")
                          .attr("class", "link")
                          .style("stroke-width", ',links$strokewidth,')
                          .style("stroke", ',links$stroke,')
                          .on("mouseover",mouseover',id,');
var node',id,' = svg',id,'.selectAll(".node")
                          .data(graph',id,'.nodes)
                          .enter().append("circle")
                          .attr("class", "node")
                          .attr("r",',nodes$radius,')
                          .style("stroke",',nodes$stroke,')
                          .style("stroke-width",',nodes$strokewidth,')
                          .style("fill", ',nodes$fill,')
                          .on("mouseover", mouseover',id,')
                          .on("mouseout", mouseout',id,')
                          .call(force',id,'.drag);
focus',id,'  = svg',id,'.append("g")
                        .attr("class","focus")
                        .style("display",null);
plaque',id,' = focus',id,'.append("rect")
                          .attr("rx",',ntb$rx,')
                          .attr("ry",',ntb$ry,')
                          .attr("style","',paste(c("fill:","stroke:","stroke-width:","fill-opacity:","stroke-opacity:"),
                                                 gsub("\"","",ntb[c("fill","stroke","strokewidth","fillopacity","strokeopacity")]),
                                                 ";",collapse="",sep=""),'");
function mouseover',id,'(d) {
    node',id,'.transition().duration(',duration,').style("stroke",',nodes$stroke,')
                                                  .style("stroke-width",',nodes$strokewidth,')
                                                  .style("fill",',nodes$fill,');
    link',id,'.transition().duration(',duration,').style("stroke",',links$stroke,')
                                                  .style("stroke-width",',links$strokewidth,');
    var mousecoords = d3.mouse(this);
    focus',id,'.transition().duration(',duration,')
               .attr("transform","translate(" + (mousecoords[0]+',offset[1],') + "," + (mousecoords[1]-',offset[2],') + ")");
    focus',id,'.selectAll("text").remove();
    var obj = d3.select(this);
    if(obj.attr("class")=="link") {
        plaque',id,'.transition().duration(',duration,').attr("height",',ltb$linespacing*length(ldet$items),')
                                              .attr("y",-',ltb$linespacing*length(ldet$items),')
                                              .style("fill",',ltb$fill,')
                                              .attr("width",',(ldet$maxitemlength+ldet$maxexprlength)*charwidth + 2*ltb$margin,');
        ',paste(sapply(1:length(ldet$items),function(i) {
            paste(
              "focus",id,'.append("text").transition().duration(0).delay(',duration,')
                          .attr("dx",',ltb$margin,')
                          .attr("dy",-',ltb$linespacing,'*',length(ldet$items)+1-i,'+',ltb$margin,')
                          .attr("stroke",',ltb$textitemcol,')
                          .text("',ldet$items[i],'");',
            sep="")
        }),collapse="\n"),'
        ',paste(sapply(1:length(ldet$items),function(i) {
            paste(
              "focus",id,'.append("text").transition().duration(0).delay(',duration,')
                          .attr("dx",',ltb$margin+charwidth*ldet$maxitemlength,')
                          .attr("dy",-',ltb$linespacing,'*',length(ldet$items)+1-i,'+',ltb$margin,')
                          .attr("stroke",',ltb$textvaluecol,')
                          .text(',ldet$expr[i],');',
            sep="")
        }),collapse="\n"),'
        obj.transition().duration(',duration,').style("stroke",',links$strokeHL,')
                                                      .style("stroke-width",',links$strokewidthHL,');
    } else {
        console.log((d3.scale.ordinal().domain([1,2,"Grp2"]).range([0,1,2]))(d.grpnm));
        plaque',id,'.transition().duration(',duration,').attr("height",',ntb$linespacing*length(ndet$items),')
                                              .attr("y",-',ntb$linespacing*length(ndet$items),')
                                              .style("fill",',ntb$fill,')
                                              .attr("width",',(ndet$maxitemlength+ndet$maxexprlength)*charwidth + 2*ntb$margin,');
        ',paste(sapply(1:length(ndet$items),function(i) {
          paste(
            "focus",id,'.append("text").transition().duration(0).delay(',duration,')
                          .attr("dx",',ntb$margin,')
                          .attr("dy",-',ntb$linespacing,'*',length(ndet$items)+1-i,'+',ntb$margin,')
                          .attr("stroke",',ntb$textitemcol,')
                          .text("',ndet$items[i],'");',
            sep="")
        }),collapse="\n"),'
        ',paste(sapply(1:length(ndet$items),function(i) {
          paste(
            "focus",id,'.append("text").transition().duration(0).delay(',duration,')
                          .attr("dx",',ntb$margin+charwidth*ndet$maxitemlength,')
                          .attr("dy",-',ntb$linespacing,'*',length(ndet$items)+1-i,'+',ntb$margin,')
                          .attr("stroke",',ntb$textvaluecol,')
                          .text(',ndet$expr[i],');',
            sep="")
        }),collapse="\n"),'
        obj.transition().duration(',duration,').attr("r",',nodes$radiusHL,')
                                                      .style("stroke-width", ',nodes$strokewidthHL,')
                                                      .style("fill", ',nodes$fillHL,')
                                                      .style("stroke", ',nodes$strokeHL,').ease("sine");
    }
    
}
function mouseout',id,'(d) {
    var obj = d3.select(this);
    obj.transition().duration(',duration,').attr("r",',nodes$radius,');
}
function mouseclick',id,'() {
    node',id,'.transition().duration(',duration,').style("stroke",',nodes$stroke,')
                                                  .style("stroke-width",',nodes$strokewidth,')
                                                  .style("fill",',nodes$fill,');
    link',id,'.transition().duration(',duration,').style("stroke",',links$stroke,')
                                                  .style("stroke-width",',links$strokewidth,');
    focus',id,'.selectAll("text").remove();
    focus',id,'.attr("transform","translate(-width',id,',-height',id,')");
} 
force',id,'.on("tick", function() {
    link',id,'.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });
   
    node',id,'.attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
});
</script>')
   
   #html <- gsub("(\n(\\s+)?\n)+","\n",html)
   return(html)
 }
 
 
# End script
 
