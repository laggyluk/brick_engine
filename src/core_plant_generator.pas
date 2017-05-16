unit core_plant_generator;

{$mode objfpc}{$H+}

{
  F	forward step
  f	forward step without drawing
  +	turn left by angle
  -	turn right by angle
  ^	pitch up by angle
  &	pitch down by angle
  /	roll right by angle
  \	roll left by angle
  |	turn around
  [	push current state of turtle onto a stack
  ]	pop state from a stack and set as current tutrle state
}

interface

uses
  Classes, SysUtils, core_main, core_types, core_utils, fgl, core_block_brush,
  VectorGeometry, core_LSystemHelper, VectorTypes,core_terrain_generator, core_render;

type

  { TPlantGenerator }

  TPlantGenerator = class
     lsys:TLSystemRulesHelper;
     procedure generate(x, y: integer; angle: single; iter: integer);
     procedure fromVars(x, y: integer; rules: tstringlist; colors: tstringlist;
       const axiom: string; angle, initAngleX: single; iter: integer;
       desiredSize: TVector3f; cullChunks: boolean);
     //position, file, render to brush/chunk
     procedure fromFile(x, y: integer; const filepath: string;toBrush:boolean);
     constructor create;
     destructor destroy;
  end;

var
  plantGenerator:TPlantGenerator;

implementation

{ TPlantGenerator }

procedure TPlantGenerator.generate(x, y: integer; angle: single; iter: integer);
var xx,i,j,yy,zz:integer;
    lodyzka,tops,flower:eBlockTypes;
    tree:string;
    s:string;
    desiredSize,size:tvector3f;

begin
  xx:=x mod chunk_width;
  zz:=y mod chunk_width;
  //find first top nonempty block
  for yy:=chunk_height-1 downto 0 do
    if core.chunks[x div chunk_width, y div chunk_width].blocks[xx,yy,zz]<>byte(btnone) then break;
  //log(inttostr(yy));
  //randomize lodyzka material
  lodyzka:=pickRandomMineral(mineralsPlants);
  tops:=pickRandomMineral(mineralsPlants);
  lsys:=TLSystemRulesHelper.Create;
  //generate some shape
  lsys.AddRule('X', '0F-[2[X]+3X]+2F[3+FX]-X');
  lsys.AddRule('F', 'FF');
  s:=lsys.Apply('X', 6);
  lsys.offset[0]:=x;
  lsys.offset[1]:=chunk_height2;
  lsys.offset[2]:=y;
  lsys.fState.AX:=90;
  setVector(size,1,1,1);
  //will return plant size instead of drawing it
 // lsys.colors:=[btWoodBrown,btWoodLight,btFloraLight,btFlora];
 // lsys.render(s,25,size,true,false);
  //let's say we want the plant to be 10x10x10 blocks max in size
  setVector(desiredSize,50,50,50);
  //now calculate proper size
  size:=VectorDivide(desiredSize,size);
  //lsys.render(s,25,size,false,true);
  lsys.destroy;
  terrain.cullAllChunks;
end;

procedure TPlantGenerator.fromVars(x, y: integer; rules: tstringlist;
  colors: tstringlist; const axiom: string; angle, initAngleX: single;
  iter: integer;desiredSize:TVector3f;cullChunks:boolean);
var xx,i,j,yy,zz:integer;
    s,s1,s2:string;
    size:tvector3f;
begin
  xx:=x mod chunk_width;
  zz:=y mod chunk_width;
  //find first top nonempty block
  for yy:=chunk_height-1 downto 0 do
    if core.chunks[x div chunk_width, y div chunk_width].blocks[xx,yy,zz]<>byte(btnone) then break;
  lsys:=TLSystemRulesHelper.Create;
  //generate some shape
  for s in rules do begin
      s2:=s;
      s1:=eatstring(s2);
      lsys.AddRule(s2,s2);
  end;
  lsys.AddRule('X', '0F-[2[X]+3X]+2F[3+FX]-X');
  lsys.AddRule('F', 'FF');
  s:=lsys.Apply('X', 6);
  lsys.offset[0]:=x;
  lsys.offset[1]:=chunk_height2;
  lsys.offset[2]:=y;
  lsys.fState.AX:=90;
  setVector(size,1,1,1);
  //will return plant size instead of drawing it
  //lsys.colors:=[btWoodBrown,btWoodLight,btFloraLight,btFlora];
  //lsys.render(s,25,size,true,false);
  //let's say we want the plant to be 10x10x10 blocks max in size
  setVector(desiredSize,50,50,50);
  //now calculate proper size
  size:=VectorDivide(desiredSize,size);
  //lsys.render(s,25,size,false,false);
  lsys.destroy;
  terrain.cullAllChunks;
end;

procedure TPlantGenerator.fromFile(x, y: integer; const filepath: string;
  toBrush: boolean);
var xx,i,j,yy,zz,iterations:integer;
    s,s1,s2,s3,axiom:string;
    desiredSize,size:tvector3f;
    f,colors:tstringlist;
    angle,initAngle:TAffineVector;
begin
  try
  xx:=x mod chunk_width;
  zz:=y mod chunk_width;
  //find first top nonempty block
  for yy:=chunk_height-1 downto 0 do
    if core.chunks[x div chunk_width, y div chunk_width].blocks[xx,yy,zz]<>byte(btnone) then break;
  lsys:=TLSystemRulesHelper.Create;
  f:=tstringlist.create;
  colors:=tstringlist.create;
  f.LoadFromFile(filepath);
  for s3 in f do begin
     s:=s3;
     s1:=eatstring(s);
     case s1 of
     'rule':begin
        s1:=eatstring(s);
        s2:=eatstring(s);
        lsys.addRule(s1,s2);
        continue;
      end;
     'axiom':axiom:=eatstring(s);
     'iterations': iterations:=strtoint(eatstring(s));
     'size':begin
        size[0]:=strtofloat(eatstring(s));
        size[1]:=strtofloat(eatstring(s));
        size[2]:=strtofloat(eatstring(s));
      end;
     'angle':begin
        angle[0]:=strtofloat(eatstring(s));
        angle[1]:=strtofloat(eatstring(s));
        angle[2]:=strtofloat(eatstring(s));
      end;
      'initialAngle':begin
        initangle[0]:=strtofloat(eatstring(s));
        initangle[1]:=strtofloat(eatstring(s));
        initangle[2]:=strtofloat(eatstring(s));
      end;
      'colors':begin
        s1:=eatstring(s);
        while length(s1)>2 do begin
           colors.add(s1);
           s1:=eatstring(s);
        end;
      end;
    end;
  end;
  f.free;
  //lsys.AddRule('X', '0F-[2[X]+3X]+2F[3+FX]-X');
  //lsys.AddRule('F', 'FF');
  //s:=lsys.Apply('X', 6);
  lsys.offset[0]:=x;
  lsys.offset[1]:=chunk_height2;
  lsys.offset[2]:=y;
  lsys.fState.AX:=90;
  desiredSize:=size;
  setVector(size,1,1,1);
  //will return plant size instead of drawing it
  //lsys.colors:=[btWoodBrown,btWoodLight,btFloraLight,btFlora];
  //first time just dummy draw to get total size needed for it
  axiom:=lsys.Apply(axiom,iterations);
  lsys.render(axiom,25,size,colors,true,false);
  //let's say we want the plant to be 10x10x10 blocks max in size
//  setVector(desiredSize,50,50,50);
  //now calculate proper size
  size:=VectorDivide(desiredSize,size);
  lsys.render(axiom,25,size,colors,false,tobrush);
  colors.free;
  //assign that thing to editor or renderer brush
  renderer.setBrush(@lsys.brush);
  //if blockBrush<>nil then freeandnil(blockBrush); //?
  //blockBrush:=lsys.brush;
  //renderer.gizmos.brush:=true;
  ////lsys.destroy;
  //terrain.cullAllChunks;
  except
    log('plantGenerator::fromFile error. while parsing file i guess');
  end;
end;

constructor TPlantGenerator.create;
begin

end;

destructor TPlantGenerator.destroy;
begin

end;

initialization
  plantGenerator:=tplantGenerator.create;

finalization
  plantGenerator.destroy;
end.

