"use strict";

var getClassOf = Function.prototype.call.bind(Object.prototype.toString);
var which = 0;
var pi = 3.14159265;

var Things = [];

class ThingList {
  constructor(data) {
    this.start = null;
    this.end   = null;
    this.length = 0;
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
    this.length += 1;
  }
  
  remove (thing) {
    var current = this.start;
    if (current===null) {
      return 0;
    }
    if (current.thing === thing) {
      this.start = current.next;
      if(current===this.end) {
        this.end = null;
      }
      this.length -= 1;
      return(1);
    }
    while (current.next !== null) {
      if(current.next.thing === thing) {
        if(current.next===this.end) {
          this.end = current;
        }
        current.next = current.next.next;
        this.length -= 1;
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
    console.log("point distance")
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    
    return Math.sqrt(dx*dx + dy*dy);
  }
  
  add (arr) {
    console.log("point add")
    return {"x":this.x+arr.x,"y":this.y+arr.y}
  }
  
  shift (arr) {
    console.log("point shift")
    this.x = this.x + arr.x;
    this.y = this.y + arr.y;
  }
}

class Thing {
  constructor(parent,orient,physics,type,pars,rotation) {
    if(Array.isArray(orient)) {
      orient = {x:orient[0],y:orient[1],xvel:orient[2],yvel:orient[3],direc:orient[4]};
    }
    if(Array.isArray(physics)) {
      physics = {grav:physics[0],fric:physics[1],bnce:physics[2],mov:physics[3],drag:physics[4]};
    }
    this.x = orient.x;
    this.y = orient.y;
    this.xvel  = (orient.xvel  === undefined) ? 0 : orient.xvel;
    this.yvel  = (orient.yvel  === undefined) ? 0 : orient.yvel;
    this.direc = (orient.direc === undefined) ? -90 : orient.direc; // Default direction: up
    this.grav = (physics.grav === undefined) ? 0 : physics.grav;
    this.fric = (physics.fric === undefined) ? 0 : physics.fric;
    this.bnce = (physics.bnce === undefined) ? 0 : physics.bnce;
    this.mov  = (physics.mov  === undefined) ? 0 : physics.mov;
    this.drag = (physics.drag === undefined) ? 0 : physics.drag;
    this.rot = (rotation===undefined) ? 0 : rotation;
    switch(type) {
      case "rect":
        this.hh = pars.hh;
        this.hw = pars.hw;
        this.obj = parent.append("rect").attr("x",0-this.hw).attr("y",0-this.hh).attr("width",2*this.hw).attr("height",2*this.hh);
        break;
      case "circ":
        this.r = pars.r;
        this.obj = parent.append("circle").attr("cx",0).attr("cy",0).attr("r",this.r);
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
    var fill = (pars.color===undefined) ? "#EEFFFF" : pars.color;
    this.id = (pars.id===undefined) ? Math.ceil(Math.random(1)*1000) : pars.id;
    this.obj.attr("transform","translate("+this.x+","+this.y+")").style("fill",fill);
    this.class = "Thing";
    this.cell = Cells[Math.floor(this.y/cellheight)][Math.floor(this.x/cellwidth)];
    this.cell.movein(this);
    this.type = (type === undefined) ? "conv" : type;
    
    Things.push(this);
  }
  
  checkCell () {
    if(this.x < 0 || this.x > width || this.y < 0 || this.y > height) {
        this.remove();
        return 0;
    }
    else {
      var tmp = Cells[Math.floor(this.y/cellheight)][Math.floor(this.x/cellwidth)];
      if (tmp !== this.cell) {
        (tmp===null) ? console.log("temp is null") :1;
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
        oldx=this.x,
        oldy=this.y;
    if(this.move(x,y,relative,0))
      var coll = this.findCollisions(Cells[Math.floor(this.y/cellheight)][Math.floor(this.x/cellwidth)]);
      
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
    
    this.x = this.x*relative + x;
    this.y = this.y*relative + y;
    this.obj.attr("transform","translate("+this.x+","+this.y+")");
    
    var res = this.checkCell()
    return(res)
  }
  
  isPointInPoly(pt) {
    console.log("point add")
      pt.x = pt.x - this.x;
      pt.y = pt.y - this.y;
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
  
  findCollisions(cell,fyi) {
    if(cell===undefined)
      cell = this.cell;
    if(fyi===undefined)
      fyi = false;
    var col = 0;
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
          curcell = cell.nd;
          break;
        case 4:
          curcell = cell.nl;
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
          if(this!==node.thing) {
            //var vec = rect_rect(this,node.thing);
            var vec = getcolfun(this.type,node.thing.type)(this,node.thing);
            if(vec) {
              //console.log(vec)
              col += 1
              if(!fyi)
                this.collide(node.thing,vec);
              //return node.thing;
            }
          }
          node = node.next;
        }
      }
    }
    return col;
  }
  
  collide () {
    console.log("nothing")
    return ;
  }
  
  onStep () {
    this.obj.attr("transform","translate("+this.x+","+this.y+")");
    if(this.type=="circ") {
      //return;
    }
    if(!this.grounded)
      this.yvel += this.grav*.2;
    if (Math.abs(this.yvel) < .1 && false) {
      this.yvel = 0;
      this.grounded = 1;
    }
    if (Math.abs(this.xvel) < .001) this.xvel = 0;
    var rep = Math.ceil(Math.max(Math.abs(this.xvel)/cellwidth,Math.abs(this.yvel)/cellheight)*20);
    for (var i=0; i<rep; i++) {
      if(Things.indexOf(this) > -1) {
        var coll = this.trymove(this.xvel/rep,this.yvel/rep,1);
      }
    }
  }
  
  remove() {
    console.log("remove1",Things.length)
    this.obj.remove();
    //this.cell.moveout(this);
    Things.splice(Things.indexOf(this),1);
    console.log("remove2",Things.length)
  }
}

class Main extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Main";
    //this.image = g.append("image").attr("x",this.x1).attr("y",this.y1).attr("width",this.x2-this.x1).attr("height",this.y2-this.y1)
  }
  
  remove() {
    super.remove();
    canMove = 0;
    canShoot = 0;
    pl = 0;
  }
}

class Project extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Project"
  }
  
  collide(thing,vec) {
    this.remove();
  }
}

class Block extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Block";
  }
}

class Bll extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars) //{hh:cellheight/2,hw:cellwidth/2}
    this.class = "Bll"
    this.grounded = 0;
  }
  
  collide (thing,vec) {
    this.move(vec.x,vec.y,1);
    switch(thing.class) {
      case "Main":
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
    return ;
  }
  
  
}