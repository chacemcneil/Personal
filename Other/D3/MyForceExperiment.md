---
title: "Example Graphs"
output: html_document
---


```
Error: cannot change working directory
```


```
## Error: unused argument (Nodesize = "nodesize")
```

<meta charset="utf-8"><script src="http://d3js.org/d3.v3.min.js"></script>
<script>

var width553  = 1100,
    height553 = 900;

var color553 = d3.scale.category20();

var force553 = d3.layout.force()
                           .charge(-120)
                           .linkDistance(80)
                           .size([width553, height553]);

var svg553 = d3.select("body").append("svg")
                                 .attr("width", width553)
                                 .attr("height", height553)
                                 .on("mousedown",mouseclick553);

graph553=
{"nodes":[{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":12,"index":1,"Index":"Vertex_1","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":10,"index":2,"Index":"Vertex_2","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":3,"Index":"Vertex_3","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":15,"index":4,"Index":"Vertex_4","repel":-1200},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":5,"Index":"Vertex_5","repel":-1200},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":6,"Index":"Vertex_6","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":10,"index":7,"Index":"Vertex_7","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":7,"index":8,"Index":"Vertex_8","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":13,"index":9,"Index":"Vertex_9","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":10,"Index":"Vertex_10","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":11,"Index":"Vertex_11","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":14,"index":12,"Index":"Vertex_12","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":16,"index":13,"Index":"Vertex_13","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":5,"index":14,"Index":"Vertex_14","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":3,"index":15,"Index":"Vertex_15","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":16,"Index":"Vertex_16","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":3,"index":17,"Index":"Vertex_17","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":1,"index":18,"Index":"Vertex_18","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":19,"Index":"Vertex_19","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":14,"index":20,"Index":"Vertex_20","repel":-120}],"links":[{"source":3,"target":6,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":2,"target":14,"value":2,"visitstowebsite":961,"text":"Link"},
{"source":13,"target":19,"value":4,"visitstowebsite":4,"text":"Line"},
{"source":2,"target":13,"value":1,"visitstowebsite":4,"text":"Link"},
{"source":7,"target":14,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":0,"target":1,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":5,"target":19,"value":3,"visitstowebsite":961,"text":"Line"},
{"source":10,"target":18,"value":1,"visitstowebsite":961,"text":"Link"},
{"source":7,"target":13,"value":4,"visitstowebsite":225,"text":"Link"},
{"source":14,"target":19,"value":4,"visitstowebsite":4,"text":"Edge"},
{"source":4,"target":17,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":3,"target":18,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":4,"target":15,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":18,"target":19,"value":2,"visitstowebsite":961,"text":"Link"}]}
;

force553.nodes(graph553.nodes)
          .links(graph553.links)
          .start();

var link553 = svg553.selectAll(".link")
                          .data(graph553.links)
                          .enter().append("line")
                          .attr("class", "link")
                          .style("stroke-width", 5)
                          .style("stroke", "gray")
                          .on("mouseover",mouseover553)
                          //.on("mouseover",mouseover553)
                          ;

var node553 = svg553.selectAll(".node")
                          .data(graph553.nodes)
                          .enter().append("circle")
                          .attr("class", "node")
                          .attr("r",10)
                          .style("stroke","white")
                          .style("stroke-width",1.5)
                          .style("fill", "blue")
                          .on("mouseover",mouseover553)
                          .on("mouseout",mouseout553)
                          //.on("mouseover",mouseover553)
                          //.on("mouseout", mouseout553)
                          .call(force553.drag);

focus553  = svg553.append("g")
                        .attr("class","focus")
                        .style("display",null);

plaque553 = focus553.append("rect")
                          .attr("rx",3)
                          .attr("ry",3)
                          .attr("style","fill:lightblue;stroke:gray;stroke-width:2;fill-opacity:0.6;stroke-opacity:0.7;");

function mouseover553(d) {
    node553.transition().duration(300).style("stroke","white")
                                                  .style("stroke-width",1.5)
                                                  .style("fill","blue")
                                                  .attr("r",10);
    link553.transition().duration(300).style("stroke","gray")
                                                  .style("stroke-width",5);
    var mousecoords = d3.mouse(this);
    var obj = d3.select(this);
    focus553.selectAll("text").remove();
    if(obj.attr("class")=="link") {
    focus553.transition().duration(300)
               .attr("transform","translate(" + ((Math.round(obj.attr("x1"))+Math.round(obj.attr("x2")))/2+12) + "," + 
                                                ((Math.round(obj.attr("y1"))+Math.round(obj.attr("y2")))/2+-12) + ")");
        plaque553.transition().duration(300).attr("height",60)
                                              .attr("y",-60)
                                              .style("fill","green")
                                              .attr("width",177);
        focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text("value: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text(d.value);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("visitstowebsite: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(d.visitstowebsite);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("text: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(d.text);
        obj.transition().duration(300).style("stroke","red")
                                                      .style("stroke-width",6);
    } else {
    focus553.transition().duration(300)
               .attr("transform","translate(" + (Math.round(obj.attr("cx"))+12) + "," + (Math.round(obj.attr("cy"))+-12) + ")");
        plaque553.transition().duration(300).attr("height",140)
                                              .attr("y",-140)
                                              .style("fill","lightblue")
                                              .attr("width",163);
        focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*7+15)
                          .attr("stroke","black")
                          .text("type: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*7+15)
                          .attr("stroke","black")
                          .text(d.type);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*6+15)
                          .attr("stroke","black")
                          .text("group: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*6+15)
                          .attr("stroke","black")
                          .text(d.group);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*5+15)
                          .attr("stroke","black")
                          .text("grpcol: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*5+15)
                          .attr("stroke","black")
                          .text(d.grpcol);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*4+15)
                          .attr("stroke","black")
                          .text("grpnm: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*4+15)
                          .attr("stroke","black")
                          .text(d.grpnm);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text("nodesize: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text(d.nodesize);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("Index: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(d.Index);
focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("repel: ");focus553.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",85)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(d.repel);
        obj.transition().duration(300).attr("r",20)
                                                      .style("stroke-width", 2)
                                                      .style("fill", "lightblue")
                                                      .style("stroke", "red").ease("sine");
    }
}

function mouseout553(d) {
    var obj = d3.select(this);
    obj.transition().duration(300).attr("r",10);
}

function mouseclick553() {
    node553.transition().duration(300).style("stroke","white")
                                                  .style("stroke-width",1.5)
                                                  .style("fill","blue");
    link553.transition().duration(300).style("stroke","gray")
                                                  .style("stroke-width",5);
    focus553.selectAll("text").remove();
    focus553.attr("transform","translate(" + (-width553-10) + "," + (-height553-10) + ")");
}

force553.on("tick", function() {
    link553.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });
    
    node553.attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
});

</script>
<script>

var width907  = 1100,
    height907 = 900;

var color907 = d3.scale.category20();

var force907 = d3.layout.force()
                           .charge(-200)
                           .linkDistance(180)
                           .size([width907, height907]);

var svg907 = d3.select("body").append("svg")
                                 .attr("width", width907)
                                 .attr("height", height907)
                                 .on("mousedown",mouseclick907);

graph907=
{"nodes":[{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":12,"index":1,"Index":"Vertex_1","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":10,"index":2,"Index":"Vertex_2","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":3,"Index":"Vertex_3","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":15,"index":4,"Index":"Vertex_4","repel":-1200},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":5,"Index":"Vertex_5","repel":-1200},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":6,"Index":"Vertex_6","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":10,"index":7,"Index":"Vertex_7","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":7,"index":8,"Index":"Vertex_8","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":13,"index":9,"Index":"Vertex_9","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":10,"Index":"Vertex_10","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":11,"Index":"Vertex_11","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":14,"index":12,"Index":"Vertex_12","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":16,"index":13,"Index":"Vertex_13","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":5,"index":14,"Index":"Vertex_14","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":3,"index":15,"Index":"Vertex_15","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":16,"Index":"Vertex_16","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":3,"index":17,"Index":"Vertex_17","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":1,"index":18,"Index":"Vertex_18","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":19,"Index":"Vertex_19","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":14,"index":20,"Index":"Vertex_20","repel":-120}],"links":[{"source":3,"target":6,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":2,"target":14,"value":2,"visitstowebsite":961,"text":"Link"},
{"source":13,"target":19,"value":4,"visitstowebsite":4,"text":"Line"},
{"source":2,"target":13,"value":1,"visitstowebsite":4,"text":"Link"},
{"source":7,"target":14,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":0,"target":1,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":5,"target":19,"value":3,"visitstowebsite":961,"text":"Line"},
{"source":10,"target":18,"value":1,"visitstowebsite":961,"text":"Link"},
{"source":7,"target":13,"value":4,"visitstowebsite":225,"text":"Link"},
{"source":14,"target":19,"value":4,"visitstowebsite":4,"text":"Edge"},
{"source":4,"target":17,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":3,"target":18,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":4,"target":15,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":18,"target":19,"value":2,"visitstowebsite":961,"text":"Link"}]}
;

force907.nodes(graph907.nodes)
          .links(graph907.links)
          .start();

var link907 = svg907.selectAll(".link")
                          .data(graph907.links)
                          .enter().append("line")
                          .attr("class", "link")
                          .style("stroke-width", 2)
                          .style("stroke", "indianred")
                          .on("mouseover",mouseover907)
                          //.on("mouseover",mouseover907)
                          ;

var node907 = svg907.selectAll(".node")
                          .data(graph907.nodes)
                          .enter().append("circle")
                          .attr("class", "node")
                          .attr("r",10)
                          .style("stroke","white")
                          .style("stroke-width",3)
                          .style("fill", "seagreen")
                          .on("mouseover",mouseover907)
                          .on("mouseout",mouseout907)
                          //.on("mouseover",mouseover907)
                          //.on("mouseout", mouseout907)
                          .call(force907.drag);

focus907  = svg907.append("g")
                        .attr("class","focus")
                        .style("display",null);

plaque907 = focus907.append("rect")
                          .attr("rx",3)
                          .attr("ry",3)
                          .attr("style","fill:white;stroke:navy;stroke-width:2;fill-opacity:0.9;stroke-opacity:1;");

function mouseover907(d) {
    node907.transition().duration(1200).style("stroke","white")
                                                  .style("stroke-width",3)
                                                  .style("fill","seagreen")
                                                  .attr("r",10);
    link907.transition().duration(1200).style("stroke","indianred")
                                                  .style("stroke-width",2);
    var mousecoords = d3.mouse(this);
    var obj = d3.select(this);
    focus907.selectAll("text").remove();
    if(obj.attr("class")=="link") {
    focus907.transition().duration(1200)
               .attr("transform","translate(" + ((Math.round(obj.attr("x1"))+Math.round(obj.attr("x2")))/2+30) + "," + 
                                                ((Math.round(obj.attr("y1"))+Math.round(obj.attr("y2")))/2+-30) + ")");
        plaque907.transition().duration(1200).attr("height",40)
                                              .attr("y",-40)
                                              .style("fill","lightgreen")
                                              .attr("width",107);
        focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("value: ");focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",64)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(d.value);
focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("text: ");focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",64)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(d.text);
        obj.transition().duration(1200).style("stroke","black")
                                                      .style("stroke-width",3);
    } else {
    focus907.transition().duration(1200)
               .attr("transform","translate(" + (Math.round(obj.attr("cx"))+30) + "," + (Math.round(obj.attr("cy"))+-30) + ")");
        plaque907.transition().duration(1200).attr("height",20)
                                              .attr("y",-20)
                                              .style("fill","white")
                                              .attr("width",135);
        focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","brown")
                          .text("type: ");focus907.append("text").attr("opacity",0).transition().duration(1200).delay(1200)
                          .attr("opacity",1)
                          .attr("dx",57)
                          .attr("dy",-20*1+15)
                          .attr("stroke","#F408ED")
                          .text(d.type);
        obj.transition().duration(1200).attr("r",20)
                                                      .style("stroke-width", 1)
                                                      .style("fill", "yellow")
                                                      .style("stroke", "red").ease("sine");
    }
}

function mouseout907(d) {
    var obj = d3.select(this);
    obj.transition().duration(1200).attr("r",10);
}

function mouseclick907() {
    node907.transition().duration(1200).style("stroke","white")
                                                  .style("stroke-width",3)
                                                  .style("fill","seagreen");
    link907.transition().duration(1200).style("stroke","indianred")
                                                  .style("stroke-width",2);
    focus907.selectAll("text").remove();
    focus907.attr("transform","translate(" + (-width907-10) + "," + (-height907-10) + ")");
}

force907.on("tick", function() {
    link907.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });
    
    node907.attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
});

</script>
<script>

var width355  = 1100,
    height355 = 900;

var color355 = d3.scale.category20();

var force355 = d3.layout.force()
                           .charge(function(d) { return ( d.repel ); })
                           .linkDistance(function(d) { return ( ( d.value * 40 ) ); })
                           .size([width355, height355]);

var svg355 = d3.select("body").append("svg")
                                 .attr("width", width355)
                                 .attr("height", height355)
                                 .on("mousedown",mouseclick355);

graph355=
{"nodes":[{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":12,"index":1,"Index":"Vertex_1","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":10,"index":2,"Index":"Vertex_2","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":3,"Index":"Vertex_3","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":15,"index":4,"Index":"Vertex_4","repel":-1200},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":5,"Index":"Vertex_5","repel":-1200},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":6,"Index":"Vertex_6","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":10,"index":7,"Index":"Vertex_7","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":7,"index":8,"Index":"Vertex_8","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":13,"index":9,"Index":"Vertex_9","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":10,"Index":"Vertex_10","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":11,"Index":"Vertex_11","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":14,"index":12,"Index":"Vertex_12","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":16,"index":13,"Index":"Vertex_13","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":5,"index":14,"Index":"Vertex_14","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":3,"index":15,"Index":"Vertex_15","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":16,"Index":"Vertex_16","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":3,"index":17,"Index":"Vertex_17","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":1,"index":18,"Index":"Vertex_18","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":19,"Index":"Vertex_19","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":14,"index":20,"Index":"Vertex_20","repel":-120}],"links":[{"source":3,"target":6,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":2,"target":14,"value":2,"visitstowebsite":961,"text":"Link"},
{"source":13,"target":19,"value":4,"visitstowebsite":4,"text":"Line"},
{"source":2,"target":13,"value":1,"visitstowebsite":4,"text":"Link"},
{"source":7,"target":14,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":0,"target":1,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":5,"target":19,"value":3,"visitstowebsite":961,"text":"Line"},
{"source":10,"target":18,"value":1,"visitstowebsite":961,"text":"Link"},
{"source":7,"target":13,"value":4,"visitstowebsite":225,"text":"Link"},
{"source":14,"target":19,"value":4,"visitstowebsite":4,"text":"Edge"},
{"source":4,"target":17,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":3,"target":18,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":4,"target":15,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":18,"target":19,"value":2,"visitstowebsite":961,"text":"Link"}]}
;

force355.nodes(graph355.nodes)
          .links(graph355.links)
          .start();

var link355 = svg355.selectAll(".link")
                          .data(graph355.links)
                          .enter().append("line")
                          .attr("class", "link")
                          .style("stroke-width", 5)
                          .style("stroke", "gray")
                          .on("mouseover",mouseover355)
                          //.on("mouseover",mouseover355)
                          ;

var node355 = svg355.selectAll(".node")
                          .data(graph355.nodes)
                          .enter().append("circle")
                          .attr("class", "node")
                          .attr("r",function(d) { return ( ( Math.sqrt( d.nodesize ) * 10 ) ); })
                          .style("stroke","black")
                          .style("stroke-width",function(d) { return ( ( d.group - 0.5 ) ); })
                          .style("fill", function(d) { return ( d.grpcol ); })
                          .on("mouseover",mouseover355)
                          .on("mouseout",mouseout355)
                          //.on("mouseover",mouseover355)
                          //.on("mouseout", mouseout355)
                          .call(force355.drag);

focus355  = svg355.append("g")
                        .attr("class","focus")
                        .style("display",null);

plaque355 = focus355.append("rect")
                          .attr("rx",3)
                          .attr("ry",3)
                          .attr("style","fill:lightblue;stroke:gray;stroke-width:2;fill-opacity:0.6;stroke-opacity:0.7;");

function mouseover355(d) {
    node355.transition().duration(100).style("stroke","black")
                                                  .style("stroke-width",function(d) { return ( ( d.group - 0.5 ) ); })
                                                  .style("fill",function(d) { return ( d.grpcol ); })
                                                  .attr("r",function(d) { return ( ( Math.sqrt( d.nodesize ) * 10 ) ); });
    link355.transition().duration(100).style("stroke","gray")
                                                  .style("stroke-width",5);
    var mousecoords = d3.mouse(this);
    var obj = d3.select(this);
    focus355.selectAll("text").remove();
    if(obj.attr("class")=="link") {
    focus355.transition().duration(100)
               .attr("transform","translate(" + ((Math.round(obj.attr("x1"))+Math.round(obj.attr("x2")))/2+12) + "," + 
                                                ((Math.round(obj.attr("y1"))+Math.round(obj.attr("y2")))/2+-12) + ")");
        plaque355.transition().duration(100).attr("height",40)
                                              .attr("y",-40)
                                              .style("fill","green")
                                              .attr("width",275);
        focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("Incr group: ");focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",99)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(( ( d.value + 5 ) + "cm" ));
focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("Length: ");focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",99)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(( d.value + " * constant value" ));
        obj.transition().duration(100).style("stroke","red")
                                                      .style("stroke-width",function(d) { return ( ( ( ( d.value + 3 ) ) / 2 ) ); });
    } else {
    focus355.transition().duration(100)
               .attr("transform","translate(" + (Math.round(obj.attr("cx"))+12) + "," + (Math.round(obj.attr("cy"))+-12) + ")");
        plaque355.transition().duration(100).attr("height",60)
                                              .attr("y",-60)
                                              .style("fill","lightblue")
                                              .attr("width",282);
        focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text("Renamed: ");focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",141)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text(d.Index);
focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("Size minus group: ");focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",141)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(( d.nodesize - d.group ));
focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("Shifted index: ");focus355.append("text").attr("opacity",0).transition().duration(100).delay(100)
                          .attr("opacity",1)
                          .attr("dx",141)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(( ( d.index + "" ) + 3 ));
        obj.transition().duration(100).attr("r",function(d) { return ( ( Math.sqrt( d.nodesize ) * 10 ) ); })
                                                      .style("stroke-width", 2)
                                                      .style("fill", function(d) { return ( d.grpcol ); })
                                                      .style("stroke", "#AA0522").ease("sine");
    }
}

function mouseout355(d) {
    var obj = d3.select(this);
    obj.transition().duration(100).attr("r",function(d) { return ( ( Math.sqrt( d.nodesize ) * 10 ) ); });
}

function mouseclick355() {
    node355.transition().duration(100).style("stroke","black")
                                                  .style("stroke-width",function(d) { return ( ( d.group - 0.5 ) ); })
                                                  .style("fill",function(d) { return ( d.grpcol ); });
    link355.transition().duration(100).style("stroke","gray")
                                                  .style("stroke-width",5);
    focus355.selectAll("text").remove();
    focus355.attr("transform","translate(" + (-width355-10) + "," + (-height355-10) + ")");
}

force355.on("tick", function() {
    link355.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });
    
    node355.attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
});

</script>
<script>

var width625  = 1100,
    height625 = 900;

var color625 = d3.scale.category20();

var force625 = d3.layout.force()
                           .charge(-120)
                           .linkDistance(80)
                           .size([width625, height625]);

var svg625 = d3.select("body").append("svg")
                                 .attr("width", width625)
                                 .attr("height", height625)
                                 .on("mousedown",mouseclick625);

graph625=
{"nodes":[{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":12,"index":1,"Index":"Vertex_1","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":10,"index":2,"Index":"Vertex_2","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":3,"Index":"Vertex_3","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":15,"index":4,"Index":"Vertex_4","repel":-1200},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":5,"Index":"Vertex_5","repel":-1200},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":6,"Index":"Vertex_6","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":10,"index":7,"Index":"Vertex_7","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":7,"index":8,"Index":"Vertex_8","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":13,"index":9,"Index":"Vertex_9","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":15,"index":10,"Index":"Vertex_10","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":11,"Index":"Vertex_11","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":14,"index":12,"Index":"Vertex_12","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":16,"index":13,"Index":"Vertex_13","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":5,"index":14,"Index":"Vertex_14","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":3,"index":15,"Index":"Vertex_15","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":11,"index":16,"Index":"Vertex_16","repel":-120},
{"type":1,"group":1,"grpcol":"yellow","grpnm":1,"nodesize":3,"index":17,"Index":"Vertex_17","repel":-120},
{"type":3,"group":3,"grpcol":"yellow","grpnm":3,"nodesize":1,"index":18,"Index":"Vertex_18","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":15,"index":19,"Index":"Vertex_19","repel":-120},
{"type":2,"group":2,"grpcol":"slateblue","grpnm":2,"nodesize":14,"index":20,"Index":"Vertex_20","repel":-120}],"links":[{"source":3,"target":6,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":2,"target":14,"value":2,"visitstowebsite":961,"text":"Link"},
{"source":13,"target":19,"value":4,"visitstowebsite":4,"text":"Line"},
{"source":2,"target":13,"value":1,"visitstowebsite":4,"text":"Link"},
{"source":7,"target":14,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":0,"target":1,"value":3,"visitstowebsite":4,"text":"Line"},
{"source":5,"target":19,"value":3,"visitstowebsite":961,"text":"Line"},
{"source":10,"target":18,"value":1,"visitstowebsite":961,"text":"Link"},
{"source":7,"target":13,"value":4,"visitstowebsite":225,"text":"Link"},
{"source":14,"target":19,"value":4,"visitstowebsite":4,"text":"Edge"},
{"source":4,"target":17,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":3,"target":18,"value":1,"visitstowebsite":225,"text":"Link"},
{"source":4,"target":15,"value":3,"visitstowebsite":225,"text":"Edge"},
{"source":18,"target":19,"value":2,"visitstowebsite":961,"text":"Link"}]}
;

force625.nodes(graph625.nodes)
          .links(graph625.links)
          .start();

var link625 = svg625.selectAll(".link")
                          .data(graph625.links)
                          .enter().append("line")
                          .attr("class", "link")
                          .style("stroke-width", 5)
                          .style("stroke", "gray")
                          
                          //.on("mouseover",mouseover625)
                          ;

var node625 = svg625.selectAll(".node")
                          .data(graph625.nodes)
                          .enter().append("circle")
                          .attr("class", "node")
                          .attr("r",10)
                          .style("stroke","white")
                          .style("stroke-width",1.5)
                          .style("fill", "blue")
                          .on("mouseover",mouseover625)
                          .on("mouseout",mouseout625)
                          //.on("mouseover",mouseover625)
                          //.on("mouseout", mouseout625)
                          .call(force625.drag);

focus625  = svg625.append("g")
                        .attr("class","focus")
                        .style("display",null);

plaque625 = focus625.append("rect")
                          .attr("rx",3)
                          .attr("ry",3)
                          .attr("style","fill:lightblue;stroke:gray;stroke-width:2;fill-opacity:0;stroke-opacity:0;");

function mouseover625(d) {
    node625.transition().duration(300).style("stroke","white")
                                                  .style("stroke-width",1.5)
                                                  .style("fill","blue")
                                                  .attr("r",10);
    link625.transition().duration(300).style("stroke","gray")
                                                  .style("stroke-width",5);
    var mousecoords = d3.mouse(this);
    var obj = d3.select(this);
    focus625.selectAll("text").remove();
    if(obj.attr("class")=="link") {
    focus625.transition().duration(300)
               .attr("transform","translate(" + ((Math.round(obj.attr("x1"))+Math.round(obj.attr("x2")))/2+15) + "," + 
                                                ((Math.round(obj.attr("y1"))+Math.round(obj.attr("y2")))/2+7) + ")");
        plaque625.transition().duration(300).attr("height",60)
                                              .attr("y",-60)
                                              .style("fill","green")
                                              .attr("width",177);
        focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text("value: ");focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*3+15)
                          .attr("stroke","black")
                          .text(d.value);
focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text("visitstowebsite: ");focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*2+15)
                          .attr("stroke","black")
                          .text(d.visitstowebsite);
focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("text: ");focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",134)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(d.text);
        obj.transition().duration(300).style("stroke","red")
                                                      .style("stroke-width",6);
    } else {
    focus625.transition().duration(300)
               .attr("transform","translate(" + (Math.round(obj.attr("cx"))+15) + "," + (Math.round(obj.attr("cy"))+7) + ")");
        plaque625.transition().duration(300).attr("height",20)
                                              .attr("y",-20)
                                              .style("fill","lightblue")
                                              .attr("width",93);
        focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text("");focus625.append("text").attr("opacity",0).transition().duration(300).delay(300)
                          .attr("opacity",1)
                          .attr("dx",15)
                          .attr("dy",-20*1+15)
                          .attr("stroke","black")
                          .text(d.Index);
        obj.transition().duration(300).attr("r",20)
                                                      .style("stroke-width", 2)
                                                      .style("fill", "lightblue")
                                                      .style("stroke", "red").ease("sine");
    }
}

function mouseout625(d) {
    var obj = d3.select(this);
    obj.transition().duration(300).attr("r",10);
}

function mouseclick625() {
    node625.transition().duration(300).style("stroke","white")
                                                  .style("stroke-width",1.5)
                                                  .style("fill","blue");
    link625.transition().duration(300).style("stroke","gray")
                                                  .style("stroke-width",5);
    focus625.selectAll("text").remove();
    focus625.attr("transform","translate(" + (-width625-10) + "," + (-height625-10) + ")");
}

force625.on("tick", function() {
    link625.attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; });
    
    node625.attr("cx", function(d) { return d.x; })
              .attr("cy", function(d) { return d.y; });
});

</script>
