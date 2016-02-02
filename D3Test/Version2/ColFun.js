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
  return fun;
}

function projectvec (vec1x,vec1y,vec2x,vec2y) {
  // project vec1 onto vec2
  if(vec2x==0 && vec2y==0) {
    console.log("div by 0")
    return {x:0, y:0};
  }
  var f = (vec1x*vec2x + vec1y*vec2y)/(vec2x*vec2x + vec2y*vec2y);
  return {x:f*vec2x, y:f*vec2y};
}

// requires x,y (center pt), hw,hh (half-width and half-height)
function rect_rect (one,two) {
  var h = Math.max(one.hw + two.hw - Math.abs(one.x - two.x),0),
      v = Math.max(one.hh + two.hh - Math.abs(one.y - two.y),0);
  if (h<=margin || v<=margin) {
    return 0;
  }
  else {
    var res = (h < v) ? {x:h*Math.sign(one.x-two.x),y:0} : {x:0,y:v*Math.sign(one.y-two.y)};
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
    console.log("corner bounce",Math.floor(circ.x/cellwidth),Math.floor(rect.x/cellwidth))
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


function sepvec (one,two) {
  // Get separation vector (smallest vector to bring objects apart)
  if(one.type=="rect" && two.type=="rect") {
    return rect_rect(one,two);
  }
}


function py_py (one,two) {
  return 0;
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



