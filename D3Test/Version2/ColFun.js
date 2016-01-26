"use strict";
// col functions

function projectvec (vec1x,vec1y,vec2x,vec2y) {
  // project vec1 onto vec2
  var f = (vec1x*vec2x + vec1y*vec2y)/(vec2x*vec2x + vec2y*vec2y);
  return {x:f*vec2x, y:f*vec2y};
}

function sepvec (one,two) {
  // Get separation vector (smallest vector to bring objects apart)
  if(one.type=="rect" && two.type=="rect") {
    return rect_rect(one,two);
  }
}

// requires x,y (center pt), hw,hh (half-width and half-height)
function rect_rect (one,two) {
  var h = Math.max(one.hw + two.hw - Math.abs(one.pos.x - two.pos.x),0)*Math.sign(one.pos.x-two.pos.x),
      v = Math.max(one.hh + two.hh - Math.abs(one.pos.y - two.pos.y),0)*Math.sign(one.pos.y-two.pos.y);
  if (h==0 || v==0) {
    return 0;
  }
  else {
    var res = (Math.abs(h) < Math.abs(v)) ? {x:h,y:0} : {x:0,y:v};
    console.log("h",h,one.pos.x,two.pos.x);
    console.log("v",v,one.pos.y,two.pos.y);
    return res;
  }
}

// requires x,y (center pt), r (radius)
function circ_circ (one,two) {
  var diffx = one.pos.x - two.pos.x,
      diffy = one.pos.y - two.pos.y,
      dist = Math.sqrt(diffx*diffx + diffy*diffy);
  if (one.r+two.r > dist)
    return 0;
  else
    return [diffx/dist*(one.r+two.r-dist),diffy/dist*(one.r+two.r-dist)];
}

// requires only what is needed for above functions
function rect_circ (rect,circ) {
  
  
  return 0;
}

function py_py (one,two) {
  return 0;
}

function proj (thing,vec) {
  var cp = (thing.pos.x*vec.y+thing.pos.y*vec.x)/(vec.x*vec.x+vec.y*vec.y);
  var range;
  switch (thing.type) {
    case "rect":
      var half = ((thing.pos.x+thing.hw)*vec.y+thing.pos.y*vec.x)/(vec.x*vec.x+vec.y*vec.y) +
                 (thing.pos.x*vec.y+(thing.pos.y+thing.hh*Math.sign(-vec.y/vec.x))*vec.x)/(vec.x*vec.x+vec.y*vec.y);
      range = [cp-half,cp+half];
      break;
    case "circ":
      range = [cp-thing.r,cp+thing.r];
      break;
    case "poly":
      
      break;
  }
  
  thing.pos.x*thing.pos.x + thing.pos.y*thing.pos.y
  
}



