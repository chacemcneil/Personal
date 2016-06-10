"use strict";
// col functions

var margin = 0.001;

function getcolfun (type1,type2) {
  if (type1=="rect" && type2=="rect")
    var fun = rect_rect;
  else if (type1=="rect" && type2=="circ")
    var fun = rect_circ;
  else if (type1=="circ" && type2=="rect")
    var fun = circ_rect;
  else if (type1=="circ" && type2=="circ")
    var fun = circ_circ;
  else if (type1=="poly" && type2=="poly")
    var fun = poly_poly;
  else if (type1=="circ" && type2=="poly")
    var fun = circ_poly;
  else if (type1=="poly" && type2=="circ")
    var fun = poly_circ;
  else if (type1=="rect" && type2=="poly")
    var fun = rect_poly;
  else if (type1=="poly" && type2=="rect")
    var fun = poly_rect;
  return fun;
}

function projectvec (vec1x,vec1y,vec2x,vec2y) {
  // project vec1 onto vec2
  if(vec2x==0 && vec2y==0) {
    //console.log("div by 0")
    return {x:0, y:0};
  }
  var f = (vec1x*vec2x + vec1y*vec2y)/(vec2x*vec2x + vec2y*vec2y);
  return {x:f*vec2x, y:f*vec2y};
}

function rightvec (vecx,vecy) {
  return {x:-vecy,y:vecx};
}

// requires x,y (center pt), hw,hh (half-width and half-height)
function rect_rect (one,two) {
  var h = Math.max(one.hw + two.hw - Math.abs(one.x - two.x),0),
      v = Math.max(one.hh + two.hh - Math.abs(one.y - two.y),0);
  if (h<=margin || v<=margin) {
    return 0;
  }
  else {
    var res = (h < v) ? {x:(h+margin)*Math.sign(one.x-two.x),y:0} : {x:0,y:(v+margin)*Math.sign(one.y-two.y)};
    return res;
  }
}

function circ_rect (circ,rect) {
  var ix  = (circ.x < rect.x-rect.hw) ? -1 : (circ.x < rect.x+rect.hw) ? 0 : 1,
      iy  = (circ.y < rect.y-rect.hh) ? -1 : (circ.y < rect.y+rect.hh) ? 0 : 1;
  switch (2*ix+3*iy) {
    case -5:
      var dx = circ.x - (rect.x-rect.hw),
          dy = circ.y - (rect.y-rect.hh),
          d  = circ.r - Math.sqrt(dx*dx+dy*dy),
          r  = d/Math.sqrt(dx*dx+dy*dy);
      dx = r*dx;
      dy = r*dy;
      break;
    case -3:
      var dx = 0,
          dy = (rect.y - circ.y) - (circ.r + rect.hh),
          d  = -dy;
      break;
    case -1:
      var dx = circ.x - (rect.x+rect.hw),
          dy = circ.y - (rect.y-rect.hh),
          d  = circ.r - Math.sqrt(dx*dx+dy*dy),
          r  = d/Math.sqrt(dx*dx+dy*dy);
      dx = r*dx;
      dy = r*dy;
      break;
    case -2:
      var dx = (rect.x - circ.x) - (circ.r + rect.hw),
          dy = 0,
          d  = -dx;
      break;
    case  0:
      var m  = rect.hh/rect.hw,
          dx = ((circ.y > m*circ.x + rect.y - m*rect.x)  ==  (circ.y < -m*circ.x + rect.y + m*rect.x))*
                (rect.hw + circ.r - Math.abs(rect.x-circ.x))*Math.sign(circ.x-rect.x),
          dy = ((circ.y > m*circ.x + rect.y - m*rect.x)  !=  (circ.y < -m*circ.x + rect.y + m*rect.x))*
                (rect.hh + circ.r - Math.abs(rect.y-circ.y))*Math.sign(circ.y-rect.y),
          d  = Math.max(Math.abs(dx),Math.abs(dy));
      break;
    case  2:
      var dx = (circ.r + rect.hw) - (circ.x - rect.x),
          dy = 0,
          d  = dx;
      break;
    case  1:
      var dx = circ.x - (rect.x-rect.hw),
          dy = circ.y - (rect.y+rect.hh),
          d  = circ.r - Math.sqrt(dx*dx+dy*dy),
          r  = d/Math.sqrt(dx*dx+dy*dy);
      dx = r*dx;
      dy = r*dy;
      break;
    case  3:
      var dx = 0,
          dy = (circ.r + rect.hh) - (circ.y - rect.y),
          d  = dy;
      break;
    case  5:
      var dx = circ.x - (rect.x+rect.hw),
          dy = circ.y - (rect.y+rect.hh),
          d  = circ.r - Math.sqrt(dx*dx+dy*dy),
          r  = d/Math.sqrt(dx*dx+dy*dy);
      dx = r*dx;
      dy = r*dy;
      break;
  }
  if (d <= margin) {
    return 0;
  }
  if (ix!=0 && iy!=0) {
    //console.log("corner bounce",Math.floor(circ.x/cellwidth),Math.floor(rect.x/cellwidth))
  }
  return {x:dx,y:dy};
}

function rect_circ (rect,circ) {
  var res = circ_rect(circ,rect)
  if (res==0)
    return res;
  return {x:-res.x,y:-res.y};
}

function circ_circ (one,two) {
  var dx = one.x-two.x,
      dy = one.y-two.y,
      R  = one.r + two.r;
  if (dx > R || dy > R || dx < -R || dy < -R) {
    return 0;
  }
  var d = R - Math.sqrt(dx*dx+dy*dy);
  if (d <= margin) {
    return 0;
  }
  else {
    return {x:d/R*dx,y:d/R*dy};
  }
}

function isClockwise(one,two,three) {
  return (one.x*three.y + two.x*one.y + three.x*two.y) - (one.x*two.y + two.x*three.y + three.x*one.y) > 0;
}
function isClockwise2(onex,oney,twox,twoy,threex,threey) {
  return (onex*threey + twox*oney + threex*twoy) - (onex*twoy + twox*threey + threex*oney) > 0;
}

function circ_poly (circ,poly) {
  // find which region the cx,cy is in, currently assuming clockwise polygons
  var outside = 0;
  for (var i=0, j=poly.points.length-1; i<poly.points.length; j=i++) {
  console.log("inside_out",circ.x,circ.y,poly.x,poly.y);
    var edge = {x:poly.points[i].x-poly.points[j].x,y:poly.points[i].y-poly.points[j].y},
        norm = rightvec(edge.x,edge.y),
        one  = {x:circ.x-poly.points[i].x-poly.x,y:circ.y-poly.points[i].y-poly.y},
        two  = {x:circ.x-poly.points[j].x-poly.x,y:circ.y-poly.points[j].y-poly.y},
        a = one.x*one.x + one.y*one.y,
        b = two.x*two.x + two.y*two.y,
        c = edge.x*edge.x + edge.y*edge.y;
    //if(isClockwise({x:poly.points[j].x+poly.x,y:poly.points[j].y+poly.y},{x:circ.x,y:circ.y},{x:poly.points[i].x+poly.x,y:poly.points[i].y+poly.y})) {
    if(isClockwise2(poly.points[j].x+poly.x,poly.points[j].y+poly.y,circ.x,circ.y,poly.points[i].x+poly.x,poly.points[i].y+poly.y)) {
      if(Math.abs(a-b) <= c) {
        var sep = projectvec(one.x,one.y,norm.x,norm.y);
        //break;
      } else if(a > b+c) {
        var sep = two;
      } else {
        var sep = one;
      }
      var dist = Math.sqrt(sep.x*sep.x + sep.y*sep.y);
      return (dist > circ.r) ? 0 : {x:sep.x*(circ.r-dist)/dist,y:sep.y*(circ.r-dist)/dist};
    }
  }
  // calculate distance accordingly
  
  return ;
}

function poly_circ (poly,circ) {
  var res = circ_poly(circ,poly)
  if (res==0)
    return res;
  return {x:-res.x,y:-res.y};
}

function poly_rect (poly,rect) {
  
  return ;
}

function rect_poly (rect,poly) {
  
  return ;
}

function poly_poly (one,two) {
  
  return;
}



///// Below functions not currently used.

function sepvec (one,two) {
  // Get separation vector (smallest vector to bring objects apart)
  if(one.type=="rect" && two.type=="rect") {
    return rect_rect(one,two);
  }
}



function proj (thing,vec) {
  var cp = (thing.x*vec.y+thing.y*vec.x)/(vec.x*vec.x+vec.y*vec.y);
  var range;
  switch (thing.type) {
    case "rect":
      var half = ((thing.x+thing.hw)*vec.y+thing.y*vec.x)/(vec.x*vec.x+vec.y*vec.y) +
                 (thing.x*vec.y+(thing.y+thing.hh*Math.sign(-vec.y/vec.x))*vec.x)/(vec.x*vec.x+vec.y*vec.y);
      range = [cp-half,cp+half];
      break;
    case "circ":
      range = [cp-thing.r,cp+thing.r];
      break;
    case "poly":
      
      break;
  }
  
  thing.x*thing.x + thing.y*thing.y
  
}



