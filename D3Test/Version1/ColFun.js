"use strict";
// col functions


// requires x,y (center pt), hw,hh (half-width and half-height)
function ab_ab (one,two) {
  h = Math.max(one.hw + two.hw - Math.abs(one.pos.x - two.pos.x),0)*Math.sign(one.pos.x-two.pos.x);
  v = Math.max(one.hh + two.hh - Math.abs(one.pos.y - two.pos.y),0)*Math.sign(one.pos.y-two.pos.y);
  if (h==0 && v==0)
    return 0;
  else
    return (Math.abs(h) < Math.abs(v)) ? [h,0] : [0,v];
  if(1);
  else if (Math.abs(h) < Math.abs(v)) {
    
    return [h,0]
  } else {
    
    return [0,v]
  }
}

// requires x,y (center pt), r (radius)
function cr_cr (one,two) {
  var diffx = one.pos.x - two.pos.x,
      diffy = one.pos.y - two.pos.y,
      dist = Math.sqrt(diffx*diffx + diffy*diffy);
  if (one.r+two.r > dist)
    return 0;
  else
    return [diffx/dist*(one.r+two.r-dist),diffy/dist*(one.r+two.r-dist)];
}

// requires only what is needed for above functions
function ab_cr (rect,circ) {
  
  
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



