"use strict";

var Things = [];

class ThingList {
  constructor(data) {
    this.start = null;
    this.end   = null;
  }
  
  makeNode () {
    return {thing:null,next:null};
  }
  
  add (thing) {
    if(this.start===null) {
      this.start = this.makeNode();
      this.end = this.start;
    }
    else {
      this.end.next = this.makeNode();
      this.end = this.end.next;
    }
    this.end.thing = thing;
  }
  
  remove (thing) {
    var current = this.start;
    if (current.thing === thing) {
      this.start = current.next;
      if(current===this.end) {
        this.end = null;
      }
      return(1);
    }
    while (current.next !== null) {
      if(current.next.thing === thing) {
        current.next = current.next.next;
        if(current.next===this.end) {
          this.end = current;
        }
        return(1);
      }
      current = current.next
    }
    return(0);
  }
}

class Cell {
  constructor() {
    this.ThingList = new ThingList()
    this.nl = null;
    this.nu = null;
    this.nr = null;
    this.nd = null;
    this.nul = null;
    this.nur = null;
    this.ndl = null;
    this.ndr = null;
  }
  
  movein(thing) {
    this.ThingList.add(thing)
  }
  
  moveout(thing) {
    this.ThingList.remove(thing)
  }
}

class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
  
  static distance(a, b) {
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    
    return Math.sqrt(dx*dx + dy*dy);
  }
  
  add (arr) {
    return {"x":this.x+arr.x,"y":this.y+arr.y}
  }
  
  shift (arr) {
    this.x = this.x + arr.x;
    this.y = this.y + arr.y;
  }
}

class Thing {
  constructor(parent,x,y,xs,ys,type) {
    this.pos = new Point(x,y);
    this.poly = [];
    this.string = "";
    for(var i =0; i < xs.length; i++) {
      this.poly.push(new Point(xs[i],ys[i]));
      this.string = this.string + " " + (xs[i]) + "," + (ys[i]);
    }
    this.x1 = xs.slice().sort(function(a,b){return a-b})[0];
    this.x2 = xs.slice().sort(function(a,b){return b-a})[0];
    this.y1 = ys.slice().sort(function(a,b){return a-b})[0];
    this.y2 = ys.slice().sort(function(a,b){return b-a})[0];
    this.obj = parent.append("polygon").attr("points",this.string).attr("transform","translate("+this.pos.x+","+this.pos.y+")")
                     .style("fill","#EEFFFF");
    this.class = "Thing";
    this.cell = Cells[Math.floor(y/cellheight)][Math.floor(x/cellwidth)];
    this.cell.movein(this);
    this.type = (type === undefined) ? "conv" : type;
    
    Things.push(this);
  }
  
  checkCell () {
    if(this.pos.x < 0 || this.pos.x > width || this.pos.y < 0 || this.pos.y > height) {
        return this.remove();
    }
    else {
      this.cell.moveout(this);
      this.cell = Cells[Math.floor(this.pos.y/cellheight)][Math.floor(this.pos.x/cellwidth)];
      this.cell.movein(this);
    }
  }
  
  trymove(x,y,relative) {
    if(relative===undefined)
      relative = 1;
    var curr = this,
        oldx=this.pos.x,
        oldy=this.pos.y;
    this.move(x,y,relative,1);
    var coll = this.isCollisionAny(Cells[Math.floor(this.pos.y/cellheight)][Math.floor(this.pos.x/cellwidth)]);
    if(coll) {
      this.move(oldx,oldy,0);
    }
    else
      this.move(0,0,1,0);
    return !coll;
  }
  
  move(x,y,relative,tentative) {
    if(relative===undefined)
      relative = 1;
    if(tentative===undefined)
      tentative = 0;
    
    this.pos.x = this.pos.x*relative + x;
    this.pos.y = this.pos.y*relative + y;
    this.obj.attr("transform","translate("+this.pos.x+","+this.pos.y+")");
    
    if(!tentative) {
      this.checkCell();
      if (this.isCollisionAny() & false) {
        //this.remove();
      }
    }
  }
  
  isPointInPoly(pt) {
      pt.x = pt.x - this.pos.x;
      pt.y = pt.y - this.pos.y;
      for(var c = false, d = false, i = -1, l = this.poly.length, j = l - 1; ++i < l; j = i) {
          ((this.poly[i].y <= pt.y && pt.y <= this.poly[j].y) || (this.poly[j].y <= pt.y && pt.y <= this.poly[i].y))
          && (pt.x <  (this.poly[j].x - this.poly[i].x) * (pt.y - this.poly[i].y) / (this.poly[j].y - this.poly[i].y) + this.poly[i].x)
          && (c = !c) ;
          ((this.poly[i].y <= pt.y && pt.y <= this.poly[j].y) || (this.poly[j].y <= pt.y && pt.y <= this.poly[i].y))
          && (pt.x <=(this.poly[j].x - this.poly[i].x) * (pt.y - this.poly[i].y) / (this.poly[j].y - this.poly[i].y) + this.poly[i].x)
          && (d = !d) 
          //if (pt.x == (this.poly[j].x - this.poly[i].x) * (pt.y - this.poly[i].y) / (this.poly[j].y - this.poly[i].y) + this.poly[i].x) {
          //if (pt.x == (this.poly[j].x - this.poly[i].x) * (pt.y - this.poly[i].y) / (this.poly[j].y - this.poly[i].y) + this.poly[i].x) {
          //  c = true;
          //  break;
          //}
      }
      return (c||d);
  }
  
  static isCollisionBox(thing1,thing2) {
    if (thing1===thing2)
      return false;
    //else
      //return ab_ab(thing1,thing2);
    return !(thing1===thing2 || thing1.y2+thing1.pos.y < thing2.y1+thing2.pos.y || thing1.y1+thing1.pos.y > thing2.y2+thing2.pos.y || 
             thing1.x2+thing1.pos.x < thing2.x1+thing2.pos.x || thing1.x1+thing1.pos.x > thing2.x2+thing2.pos.x);
  }
  
  static isCollision(thing1,thing2) {
    var inbox = Thing.isCollisionBox(thing1,thing2);
    if (inbox) {
      for(var i=0; i < thing1.poly.length; i++) {
        if(thing2.isPointInPoly(thing1.poly[i].add(thing1.pos)) )
          return 1;
      }
      for(var i=0; i < thing2.poly.length; i++) {
        if(thing1.isPointInPoly(thing2.poly[i].add(thing2.pos)) )
          return 1;
      }
      return 0;
    }
    else
      return 0;
  }
  
  isCollisionAny(cell) {
    if(cell === undefined)
      cell = this.cell;
    for(var cl = 0; cl<9; cl++) {
      var curcell;
      switch (cl) {
        case 0:
          curcell = cell;
          break;
        case 1:
          curcell = cell.nul;
          break;
        case 2:
          curcell = cell.nu;
          break;
        case 3:
          curcell = cell.nur;
          break;
        case 4:
          curcell = cell.nr;
          break;
        case 5:
          curcell = cell.ndr;
          break;
        case 6:
          curcell = cell.nd;
          break;
        case 7:
          curcell = cell.ndl;
          break;
        case 8:
          curcell = cell.nl;
          break;
      }
      if(curcell !== null) {
        var poss = curcell.ThingList
        var node = poss.start
        while(node !== null) {
          if(Thing.isCollision(this,node.thing)) {
            return node.thing;
          }
          node = node.next;
        }
      }
    }
    return 0;
  }
  
  onStep () {
    this.obj.attr("points",this.string).attr("transform","translate("+this.pos.x+","+this.pos.y+")");
    if (this.pos.x < 0 - buffer || this.pos.x > width + buffer || this.pos.y < 0 - buffer || this.pos.y > height + buffer) {
      this.remove()
    }
  }
  
  remove() {
    this.obj.remove();
    Things.splice(Things.indexOf(this),1);
  }
}

class Main extends Thing {
  constructor(parent,x,y,xs,ys,direction) {
    super(parent,x,y,xs,ys);
    if(direction===undefined)
      this.direction = 90;
    else
      this.direction = direction;
    this.class = "Main";
    this.image = g.append("image").attr("x",this.x1).attr("y",this.y1).attr("width",this.x2-this.x1).attr("height",this.y2-this.y1)
                     //.attr("xlink:href","globe.jpg").attr("transform","translate("+this.pos.x+","+this.pos.y+")");
    //<image x="0" y="0" width="236" height="200" xlink:href="globe.jpg" />
  }
  
  onStep () {
    this.obj.attr("points",this.string).attr("transform","translate("+this.pos.x+","+this.pos.y+") rotate("+this.direction+")");
    
  }
  
  remove() {
    super.remove();
    canMove = 0;
    canShoot = 0;
    pl = 0;
  }
}

class Project extends Thing {
  constructor(parent,x,y,xs,ys,direc,veloc) {
    super(parent,x,y,xs,ys);
    this.direc = direc;
    this.veloc = veloc;
  }
  
  onStep() {
    super.onStep();
    this.move(Math.cos(this.direc*pi/180)*this.veloc,Math.sin(this.direc*pi/180)*this.veloc,1,0);
  }
}

class Block extends Thing {
  constructor(parent,row,col,blk) {
    super(parent,col*cellwidth,row*cellheight,blk.xs,blk.ys);
    
  }
}

var Menys = [];
class Meny extends Thing {
  constructor(parent,x,y,xs,ys) {
    super(parent,x,y,xs,ys);
    Menys.push(this);
    this.class = "Meny"
    this.obj.style("fill","#FFEEEE")
  }
  
  remove() {
    super.remove();
    Menys.splice(Menys.indexOf(this),1)
  }
}

class Bll extends Meny {
  constructor(parent,x,y,r,xvel,yvel) {
    super(parent,x,y,[0,r,r,0],[0,0,r,r])
    this.xvel = (xvel === undefined) ? Math.random()+2 : xvel;
    this.yvel = (yvel === undefined) ? Math.random()+2 : yvel;
    console.log(this.xvel,xvel)
    this.grav = 1;
    this.bnce = .9;
    this.fric = .3;
  }
  
  onStep () {
    this.obj.attr("points",this.string).attr("transform","translate("+this.pos.x+","+this.pos.y+")");
    this.yvel += this.grav*.2;
    if (Math.abs(this.yvel) < .1) this.yvel = 0;
    if (Math.abs(this.xvel) < .001) this.xvel = 0;
    var rep = Math.ceil(Math.max(Math.abs(this.xvel)/cellwidth,Math.abs(this.yvel)/cellheight)*2)
    for (var i=0; i<rep; i++) {
      console.log(rep)
      if(!this.trymove(this.xvel/rep,this.yvel/rep,1,0)) {
        console.log(this.yvel,this.xvel);
        this.xvel -= this.fric*this.xvel;
        this.yvel = -this.yvel*this.bnce;
        break;
      }
    }
  }
}

class Clock {
  constructor(parent,time,x,y,rad) {
    this.time = time;
    this.pos = new Point(x,y);
    this.rad = rad;
    this.obj = parent.append("circle").attr("cx",0).attr("cy",0).attr("r",rad).data(hands).enter().append("rect").
    this.obj.attr("transform","translate("+x+","+y+")")
  }
  
  onStep() {
    this.time = this.time + 1;
    this.obj.attr("transform","translate("+this.pos.x+","+this.pos.y+")")
  }
}