"use strict";

var getClassOf = Function.prototype.call.bind(Object.prototype.toString);

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
  
  checkCollision() {
    var curthing = this.ThingList.start;
    while(curthing!==null) {
      var oththing = curthing.next;
      while(oththing!==null) {
        sepvec(curthing,oththing);
      }
      oththing = (this.nul===null) ? null : this.nul.start;
      while(oththing!==null) {
        sepvec(curthing,oththing);
      }
      oththing = (this.nu===null)  ? null : this.nu.start;
      while(oththing!==null) {
        sepvec(curthing,oththing);
      }
      oththing = (this.nur===null) ? null : this.nur.start;
      while(oththing!==null) {
        sepvec(curthing,oththing);
      }
      oththing = (this.nr===null)  ? null : this.nr.start;
      while(oththing!==null) {
        sepvec(curthing,oththing);
      }
      
      curthing = curthing.next;
    }
  }
  
  checkCollisionList(curthing,oththing) {
    return ;
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
  constructor(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars,rotation) {
    this.pos = new Point(x,y);
    this.xvel = (xvel === undefined) ? Math.random()+2 : xvel;
    this.yvel = (yvel === undefined) ? Math.random()+2 : yvel;
    this.direc = (direc === undefined) ? -90 : direc;
    this.grav = (grav === undefined) ? 0 : grav;
    this.fric = (fric === undefined) ? 0 : fric;
    this.bnce = (bnce === undefined) ? 0 : bnce;
    this.mov = (mov===undefined) ? 0 : mov;
    this.rot = (rotation===undefined) ? 0 : rotation;
    switch(type) {
      case "rect":
        this.hh = pars.hh;
        this.hw = pars.hw;
        this.obj = parent.append("rect").attr("x",0-this.hw).attr("y",0-this.hh).attr("width",2*this.hw).attr("height",2*this.hh);
        break;
      case "circ":
        this.rad = pars.rad;
        this.obj = parent.append("circle").attr("cx",this.pos.x).attr("cy",this.pos.y).attr("r",this.rad);
        break;
      case "poly":
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
        this.obj = parent.append("polygon").attr("points",this.string)
        break;
    }
    this.obj.attr("transform","translate("+this.pos.x+","+this.pos.y+")").style("fill","#EEFFFF");
    this.class = "Thing";
    this.cell = Cells[Math.floor(y/cellheight)][Math.floor(x/cellwidth)];
    this.cell.movein(this);
    this.type = (type === undefined) ? "conv" : type;
    
    Things.push(this);
  }
  
  checkCell () {
    if(this.pos.x < 0 || this.pos.x > width || this.pos.y < 0 || this.pos.y > height) {
        this.remove();
        return 0;
    }
    else {
      var tmp = Cells[Math.floor(this.pos.y/cellheight)][Math.floor(this.pos.x/cellwidth)];
      if (tmp !== this.cell) {
        this.cell.moveout(this);
        this.cell = tmp;
        this.cell.movein(this);
      }
      return 1;
    }
  }
  
  trymove(x,y,relative) {
    if(relative===undefined)
      relative = 1;
    var curr = this,
        oldx=this.pos.x,
        oldy=this.pos.y;
    if(this.move(x,y,relative,0))
      var coll = this.isCollisionAny(Cells[Math.floor(this.pos.y/cellheight)][Math.floor(this.pos.x/cellwidth)]);
    //if(coll) {
    //  this.move(coll.x,coll.y,0);
    //}
    //else
    //  this.move(0,0,1,0);
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
      return(this.checkCell())
      if (false & this.isCollisionAny()) {
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
    if(cell===undefined)
      cell = this.cell;
    for(var cl = 0; cl<9; cl++) {
      var curcell;
      switch (cl) {
        case 0:
          curcell = cell;
          break;
        case 1:
          curcell = cell.nu;
          break;
        case 2:
          curcell = cell.nr;
          break;
        case 3:
          curcell = cell.nl;
          break;
        case 4:
          curcell = cell.nd;
          break;
        case 5:
          curcell = cell.nul;
          break;
        case 6:
          curcell = cell.nur;
          break;
        case 7:
          curcell = cell.ndr;
          break;
        case 8:
          curcell = cell.ndl;
          break;
      }
      if(curcell !== null) {
        var poss = curcell.ThingList
        var node = poss.start
        while(node !== null) {
          var col = rect_rect(this,node.thing);
          if(col) {
            this.collide(node.thing,col);
            return node.thing;
          }
          node = node.next;
        }
      }
    }
    return 0;
  }
  
  collide () {
    return ;
  }
  
  onStep () {
    this.obj.attr("points",this.string).attr("transform","translate("+this.pos.x+","+this.pos.y+")");
    if(!this.grounded)
      this.yvel += this.grav*.2;
    if (Math.abs(this.yvel) < .1 && false) {
      this.yvel = 0;
      this.grounded = 1;
    }
    if (Math.abs(this.xvel) < .001) this.xvel = 0;
    var rep = Math.ceil(Math.max(Math.abs(this.xvel)/cellwidth,Math.abs(this.yvel)/cellheight)*10);
    for (var i=0; i<rep; i++) {
      if(Things.indexOf(this) > -1) {
        var coll = this.trymove(this.xvel/rep,this.yvel/rep,1,0);
      }
    }
  }
  
  remove() {
    this.obj.remove();
    console.log("remove");
    //this.cell.moveout(this);
    console.log("moveout");
    Things.splice(Things.indexOf(this),1);
  }
}

class Main extends Thing {
  constructor(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars) {
    super(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars);
    this.class = "Main";
    //this.image = g.append("image").attr("x",this.x1).attr("y",this.y1).attr("width",this.x2-this.x1).attr("height",this.y2-this.y1)
  }
  
  onStep () {
    super.onStep()
    
  }
  
  remove() {
    super.remove();
    canMove = 0;
    canShoot = 0;
    pl = 0;
  }
}

class Project extends Thing {
  constructor(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars) {
    console.log(this.class);
    super(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars);
    console.log(this.class);
    this.class = "Project"
    console.log(this.class);
  }
  
  onStep() {
    super.onStep();
    this.move(Math.cos(this.direc*pi/180)*this.veloc,Math.sin(this.direc*pi/180)*this.veloc,1,0);
  }
}

class Block extends Thing {
  constructor(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars) {
    super(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars);
    this.class = "Block";
  }
}

class Bll extends Thing {
  constructor(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars) {
    super(parent,x,y,xvel,yvel,direc,grav,fric,bnce,mov,type,pars) //{hh:cellheight/2,hw:cellwidth/2}
    this.class = "Bll"
    this.grounded = 0;
  }
  
  collide (thing,vec) {
    //console.log(this.class," collide with ",thing.class,vec)
    console.log("pre-collide",this.xvel,this.yvel,vec)
    this.move(vec.x,vec.y,1);
    switch(thing.class) {
      case "Block":
        //this.xvel = -this.xvel;
        //this.yvel = -this.yvel;
        //break;
        var b1 = projectvec(this.xvel,this.yvel,vec.x,vec.y),
            f1 = projectvec(this.xvel,this.yvel,-vec.y,vec.x);
        this.xvel = -b1.x + f1.x;
        this.yvel = -b1.y + f1.y;
        break;
      case "Bll":
        var b1 = projectvec(this.xvel,this.yvel,vec.x,vec.y),
            f1 = projectvec(this.xvel,this.yvel,vec.y,-vec.x),
            b2 = projectvec(thing.xvel,thing.yvel,vec.x,vec.y),
            f2 = projectvec(thing.xvel,thing.yvel,vec.y,-vec.x);
        this.xvel  = b2.x + f1.x;
        this.yvel  = b2.y + f1.y;
        thing.xvel = b1.x + f2.x;
        thing.yvel = b1.y + f2.y;
        break;
    }
    console.log("post-collide",this.xvel,this.yvel)
    return ;
  }
  
  
}