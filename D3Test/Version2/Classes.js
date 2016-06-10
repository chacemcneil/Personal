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
        this.points = [];
        this.string = "";
        var prev = {x:pars.xs[pars.xs.length-1],y:pars.ys[pars.ys.length-1]};
        this.area = 0;
        for(var i =0; i < pars.xs.length; i++) {
          var next = {x:pars.xs[i],y:pars.ys[i]};
          this.area += (next.x-prev.x)*(next.y+prev.y);
          prev = next;
          this.points.push(next);
          this.string = this.string + " " + (pars.xs[i]) + "," + (pars.ys[i]);
        }
        if(this.area < 0) {
          this.points.reverse()
        }
        //this.x1 = pars.xs.slice().sort(function(a,b){return a-b})[0];
        //this.x2 = pars.xs.slice().sort(function(a,b){return b-a})[0];
        //this.y1 = pars.ys.slice().sort(function(a,b){return a-b})[0];
        //this.y2 = pars.ys.slice().sort(function(a,b){return b-a})[0];
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
            //console.log(node.thing);
            var vec = getcolfun(this.type,node.thing.type)(this,node.thing);
            if(vec) {
              //console.log(vec)
              col += 1
              if(!fyi) { // Ignore if only checking for a collision, rather than colliding
                this.collide(node.thing,vec);
                node.thing.collided(this,{x:-vec.x,y:-vec.y});
              }
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
  
  collided () {
    //console.log("nothing")
    return ;
  }
  
  onStep () {
    this.obj.attr("transform","translate("+this.x+","+this.y+")");
    if(this.type=="circ") {
      //return;
    }
    if (Math.abs(this.yvel) < .1 && false) {
      this.yvel = 0;
      this.grounded = 1;
    }
    if (Math.abs(this.xvel) < .001 && this.xvel != 0) {
      console.log("Zeroed")
      this.xvel = 0;
    }
    //var xmove = this.xvel,
        //ymove = this.yvel - this.grav/2,
    //var ymove = (this.grav==0) ? this.yvel : -2/(this.grav)*(v1*v1-this.yvel*this.yvel);
    //var ymove = (this.grav==0) ? this.yvel : -2/(this.grav)*(this.yvel*this.yvel-(this.yvel+this.grav)*(this.yvel+this.grav));
    var ymove = (this.grav==0) ? this.yvel : -2/this.grav*(-this.grav*this.grav-2*this.yvel*this.grav);
    var rep = Math.ceil(Math.max(Math.abs(this.xvel)/cellwidth,Math.abs(ymove)/cellheight)*10);
    for (var i=0; i<rep; i++) {
      if(Things.indexOf(this) > -1) {
        var v1 = this.yvel;
        if(!this.grounded)
          this.yvel = this.yvel + this.grav/rep;
        
        var ymove = (this.grav==0) ? this.yvel/rep : -(v1*v1-this.yvel*this.yvel)/this.grav/2;
        //console.log(rep,this.grav,this.yvel,ymove);
        if(ymove != this.yvel) {
          //console.log(this.yvel,ymove)
        }
        //var coll = this.trymove(this.xvel/rep,this.yvel/rep,1);
        var coll = this.trymove(this.xvel/rep,ymove,1);
        //console.log(xmove,ymove,this.xvel,this.yvel);
        if(!coll) {
          //this.yvel = this.yvel - this.grav/rep;
        }
      }
    }
    //this.yvel = this.yvel*rep;
  }
  
  remove() {
    console.log("remove1",Things.length)
    this.obj.remove();
    this.cell.moveout(this);
    Things.splice(Things.indexOf(this),1);
    console.log("remove2",Things.length)
  }
}

class Main extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Main";
    //this.image = g.append("image").attr("x",this.x-this.hw).attr("y",this.y-this.hh)
    //                              .attr("width",2*this.hw).attr("height",2*this.hh)
  }
  
  collide(thing,vec) {
    this.x += vec.x;
    this.y += vec.y;
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
    if(thing.class=="Bll")
      thing.remove();
  }
  
  collided(thing,vec) {
    this.remove();
    if(thing.class=="Bll")
      thing.remove();
  }
  
  remove() {
    super.remove()
  }
}

class Block extends Thing {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Block";
  }
}

class Trmp extends Block {
  constructor(parent,orient,physics,type,pars) {
    super(parent,orient,physics,type,pars);
    this.class = "Trmp";
    this.color = "#DDFFFF";
    this.bnce  = 1.1;
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
    if(this.grav!=0) {
      //console.log(Math.sqrt(this.yvel*this.yvel + 2*this.grav*vec.y),vec.y,this.yvel);
      this.yvel = Math.sqrt(Math.max(this.yvel*this.yvel + 2*this.grav*vec.y,0))*Math.sign(this.yvel);
    }
    switch(thing.class) {
      case "Main":
      case "Block":
        //this.xvel = -this.xvel;
        //this.yvel = -this.yvel;
        //break;
        var b1 = projectvec(this.xvel,this.yvel,vec.x,vec.y),
            f1 = projectvec(this.xvel,this.yvel,vec.y,-vec.x);
        this.xvel = -this.bnce*b1.x + (1-this.fric)*f1.x;
        this.yvel = -this.bnce*b1.y + (1-this.fric)*f1.y;
        break;
      case "Trmp":
        //this.xvel = -this.xvel;
        //this.yvel = -this.yvel;
        //break;
        var b1 = projectvec(this.xvel,this.yvel,vec.x,vec.y),
            f1 = projectvec(this.xvel,this.yvel,vec.y,-vec.x);
        this.xvel = -thing.bnce*b1.x + f1.x;
        this.yvel = -thing.bnce*b1.y + f1.y;
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

function Make (parent,data) {
  for (var i = 0; i < data.x.length; i++) {
    switch(data.class[i]) {
      case "Block":
        var Creator = Block;
        break;
      case "Project":
        var Creator = Project;
        break;
      case "Trmp":
        var Creator = Trmp;
        break;
      case "Main":
        var Creator = Main;
        break;
      case "Bll":
        var Creator = Bll;
        break;
    }
    new Creator(parent,[data.x[i],data.y[i],data.xvel[i],data.yvel[i],data.direc[i]],[data.grav[i],data.fric[i],data.bnce[i],data.mov[i],data.drag[i]],data.type[i],
                {hh:data.hh[i],hw:data.hw[i],r:data.r[i],color:data.color[i]})
  }
}