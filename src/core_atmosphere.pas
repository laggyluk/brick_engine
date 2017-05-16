unit core_atmosphere;

{$mode objfpc}{$H+}

{
building should be a closed space with doors. two pairs of doors. there is some oxygene
inside that is used up by ppl and opening doors. oxygen can be supplied by pipes.
oxy is produced by plants
do i need building types or just stuff inside makes things tick?

generator is created in editor or sth. needs pointer to fluidMap in thread
and needs to be added to it's generators list..
}

interface

uses
  Classes, SysUtils, core_types, core_classes,fgl, VectorGeometry, core_utils,
  VectorTypes, core_chunk;

const
  {generator constantly makes oxygen. there can be a hull breach and in
  that situation oxygen should flee immedietly. ( normally oxygen vaporates in
  contact with btNone). no good way to find a hole so from time to time generator
  makes a big fart with thin oxygen. if room is not airtight then in normal update
  it will be anihilated. if secured then it will a base for oxygen to spread in
  normal operation
}
  piardInterval = 6;//seconds

type
  eSubstanceTypes = (stWater, stOxygen);

  rFluidData = record
    //flow direction
    direction:byte;
    //if 255 then no blocks in this column are 'wet', else lowest wet block index
    wet:byte;
    //number of blocks in this columns. bug potent
    //count:byte;
    //if same as current tick then skip update. to avoid updating same col
    //twice a frame
    skip:boolean;
  end;
  pFluidData = ^rFluidData;

  { TFluidMap }
  //simulates fluid flow on given area.
  //will alter all blocks of given type..
  TFluidMap = class
   private
    lastTime:single;
    substanceBase,substanceMax:byte;
    data:array of rFluidData;
    skipUpdate,working:boolean;
    //since can be updated inside a thread don't resize immiedietly but in update loop
    newWidth,
    newHeight,
    newDepth:integer;
    needResize:boolean;
    //keep track of created and deleted blocks
    totalBlocks:cardinal;
    function it(const x, z: cardinal): pFluidData;
    procedure fresize(w, h, d: integer);
   public
    //data has w*d size. h is only for constraining the simulation to a cube rather
    //than to all chunk height
    width,height,depth:integer;
    substance:set of eBlockTypes;
    //update interval in seconds
    interval:single;
    procedure update;
    procedure updateFluid;
    //normal operation. oxygen vaporates on contact with btNone
    procedure updateGasNormal;
    //'fill holes' mode. btNormal is filled with thin gas
    procedure updateGasFillHoles;
    procedure leresize(w, h, d: integer);
    procedure setWet(x, y, z: cardinal;alsoSkip:boolean=false);
    constructor create(w,h,d:integer;typ:eSubstanceTypes;updateInterval:single=1.0);
  end;
  pFluidMap=^TFluidMap;

  { TAutomataGenerator }

  TAutomataGenerator = class (TUpdateable)
  private
    fpower: eBlockTypes;
    map:pFluidMap;
    substanceBase,substanceMax:TBlockType;
    //copied from terrainGenerator to remove circ dependency
    function GetBlockInWorld(x, y, z: integer; centered: boolean): pBlockType;
    procedure SetBlockInWorld(x, y, z: integer;blok: eBlockTypes; centered,cull: boolean);
    function getPower: integer;
    procedure setPower(AValue: integer);
  public
    //position in world
    position:tVector3i;
    //type of resource generated
    substance:sMinerals;
    //how much of resource is generated on interval
    qty:integer;
    //time step between 'generates'
    interval:single;
    //time of last 'generate'
    lastUpdate:single;
    //update will be skipped n times before children get updated.
    //to simulate thickness or sth
    procedure update;override;
    property power:integer read getPower write setPower;
    constructor create(pos: TVector3i; fluidmap: pFluidMap; aPower: integer;
      timeStep: single);
    constructor create(pos: TAffineVector; fluidmap: pFluidMap;
      aPower: integer; timeStep: single);
  end;

  TGeneratorsList = specialize TFPGList<TAutomataGenerator>;
  //goes over the atmosphere and updates it

  { TFluidThread }

  TFluidThread = class(TThread)
   private
    skipUpdate:boolean;
   public
    working:boolean;
    generators:TGeneratorsList;
    procedure addGenerator(g:TAutomataGenerator);
    procedure execute;override;
    procedure pause;
    procedure resume;
    procedure resize(w, h, d: integer);
    constructor create(pausd:boolean);
    destructor destroy;override;
  end;

var
  atmosphere:TFluidThread;
  waterMap,oxygenMap:TFluidMap;

implementation

{ TFluidThread }

procedure TFluidThread.addGenerator(g: TAutomataGenerator);
begin
  generators.add(g);
end;

procedure TFluidThread.execute;
begin
  while not terminated do begin
      if not skipUpdate then waterMap.update;
      if not skipUpdate then oxygenMap.update;
      sleep(10);
  end;
end;

procedure TFluidThread.pause;
begin
  skipUpdate:=true;
  while working=true do sleep(10);
end;

procedure TFluidThread.resume;
begin
  skipUpdate:=false;
end;

procedure TFluidThread.resize(w, h, d: integer);
begin
  waterMap.leresize(w,h,d);
  oxygenMap.leresize(w,h,d);
end;

constructor TFluidThread.create(pausd: boolean);
begin
  inherited create(pausd);
                                                      //speed of visual update?
  waterMap:=TFluidMap.create(world_width,chunk_height,world_depth,stWater,0.3);
  oxygenMap:=TFluidMap.create(world_width,chunk_height,world_depth,stOxygen,0.1);
  generators:=TGeneratorsList.create;
end;

destructor TFluidThread.destroy;
var i:integer;
begin
  waterMap.free;
  oxygenMap.free;
  for i:=0 to generators.Count-1 do generators[i].free;
  generators.free;
  inherited;
end;

{ TFluidMap }

procedure TFluidMap.update;
begin
  if btWater1 in substance then updateFluid
  else //updateGasNormal;/
     updateGasFillHoles;
end;

procedure TFluidMap.updateFluid;
var
  a,am,ap,bm,bp,b,x,y,z,c,i,j
  ,xx,zz,xxm,xxp,zzm,zzp,wtf:integer;
  p:pFluidData;
  //:smallint;
  aa,bb,dif,m:byte;
  flag:boolean;
  blok,below,next:pBlockType;
  sasiedzi:array [0..3] of pBlockType;
  wety:array [0..3] of record
    x,z,xx,zz:integer;
  end;

begin
  working:=true;
  if needResize then begin
     needResize:=false;
     fresize(newWidth, newHeight,newDepth);
     exit;
  end;

  //actually update should be distributed across frames?
  //resume powinno czyscic flagi czy cos bo pauza mogla sie wwybic  polowy updatu ?

  lastTime+=deltaT;
  if lastTime<interval then exit;
  lastTime:=0;

  for z:=0 to depth-1 do
    for x:=0 to width-1 do begin
      if skipUpdate then begin
         working:=false;
         exit;
      end;
    //for i:=0 to length(data)-1 do begin
      //process only world y columns if there's water somwhere on them
      p:=@data[x+(z*world_depth)];
      //p:=it(x,z);
      //p:=@data[i];
      if (p^.skip) then begin
         p^.skip:=false;
         continue;
      end;
      if (p^.wet<255) then begin
         //update interleave
       //  p^.skip:=true;
         y:=p^.wet;
         xx:=x div chunk_width;
         zz:=z div chunk_width;
         a:=x mod chunk_width;
         b:=z mod chunk_width;
         c:=0;
         flag:=true;
         repeat
          blok:=chunksPointer^[xx,zz].pblocks[a,y,b];
          if eBlockTypes(blok^) in substance then inc(c) else begin
             inc(y);
             continue;
          end;
          if outofmap(vector3fmake(x,y,z),false) then begin
          //   blok^:=byte(btNone); //?
             inc(y);
             //p^.skip:=true;
             continue;
          end;
          below:=chunksPointer^[xx,zz].pblocks[a,y-1,b];
          //simulate flow
          //empty blok below, just drop down
          if below^=byte(btNone) then begin
             below^:=blok^;
             //setBlockAtFast(x,y-1,z,substance,false);
             blok^:=byte(btNone);
             if p^.wet>0 then p^.wet-=1;
          end else
          //block below has some substance in already. add up
          if (eBlockTypes(below^) in substance) and (byte(below^)<substanceMax) then begin
             aa:=byte(below^)-substanceBase+1;
             bb:=byte(blok^)-substanceBase+1;
             dif:=aa+bb;
             m:=substanceMax-substanceBase;
             //if sum is more than single full block then
             if dif>substanceMax-substanceBase then begin
                //bottom block gets filled up
                below^:=substanceMax;
                blok^:=substanceBase+dif-(m)-1;//(substanceBase+(dif-11));
             end
             else //means that wody wystarczy tylko na dolny blok
             if dif=m then begin
                below^:=substanceMax;
                blok^:=byte(btNone);
             end else begin
               //bottom blok gets some water and up vanishes
                below^:=(substanceBase+dif);
                blok^:=byte(btNone);
             end;
             //if (blok^=byte(btOxy1)) or (below^=byte(btOxy1))then
                //log('wtf');
             if p^.wet>0 then p^.wet-=1;
          end else begin
           //now spread to empty blocks nearby and at same level
           { TODO -cwater : watch out for world bounds.. bug on chunk edge}
           //border blocks. calculate only once for each column  pass

           if flag then begin
             xxm:=(x-1) div chunk_width;
             xxp:=(x+1) div chunk_width;
             zzm:=(z-1) div chunk_width;
             zzp:=(z+1) div chunk_width;
             ap:=(x+1) mod chunk_width;
             am:=(x-1) mod chunk_width;
             bp:=(z+1) mod chunk_width;
             bm:=(z-1) mod chunk_width;
             //since loop is randomized we don't know which wet coordinates to use
             wety[0].x:=x-1; wety[0].z:=z; wety[0].xx:=xxm; wety[0].zz:=zz;
             wety[1].x:=x+1; wety[1].z:=z; wety[1].xx:=xxp; wety[1].zz:=zz;
             wety[2].x:=x; wety[2].z:=z-1; wety[2].xx:=xx; wety[2].zz:=zzm;
             wety[3].x:=x; wety[3].z:=z+1; wety[3].xx:=xx; wety[3].zz:=zzp;

             flag:=false;
           end;
           sasiedzi[0]:=chunksPointer^[xxm,zz].pblocks[am,y,b];
           sasiedzi[1]:=chunksPointer^[xxp,zz].pblocks[ap,y,b];
           sasiedzi[2]:=chunksPointer^[xx,zzm].pblocks[a,y,bm];
           sasiedzi[3]:=chunksPointer^[xx,zzp].pblocks[a,y,bp];
           j:=XorShift(999);
           for i:=j to j+4 do begin
             wtf:=i mod 4;
             next:=sasiedzi[wtf];
             if (blok^>substanceBase) and (next^=byte(btNone)) then begin
                next^:=substanceBase;
                blok^-=1;
                setWet(wety[wtf].x,y,wety[wtf].z,true);
             end
             else
             if (blok^>substanceBase) and
                ((eBlockTypes(next^) in substance) and
                (next^<blok^))  then begin
                   next^+=1;
                   blok^-=1;
                   setWet(wety[wtf].x,y,wety[wtf].z,true);
                   chunksPointer^[wety[wtf].xx,wety[wtf].zz].reBuild:=true;
                   //terrain.cullBlocksInChunk(chunksPointer,wety[wtf].xx,wety[wtf].zz,false)
                end;
           end;

         end;
       inc(y);
       //skip out of loop if reached count of blocks marked for processing
       //or reached world top
       until //(c=0) or
         (y=chunk_height-1)or (skipUpdate);
       if c=0 then p^.wet:=255;
       chunksPointer^[xx,zz].reBuild:=true;
       //terrain.cullBlocksInChunk(chunksPointer,xx,zz,false);
      end;
    end;
end;

procedure TFluidMap.updateGasNormal;
var
  a,am,ap,bm,bp,b,x,y,z,c,i,j
  ,xx,zz,xxm,xxp,zzm,zzp,wtf:integer;
  p:pFluidData;
  //:smallint;
  dif,m:byte;
  flag:boolean;
  blok,next:pBlockType;
  sasiedzi:array [0..5] of pBlockType;
  wety:array [0..5] of record
    x,z,y,xx,zz:integer;
  end;
begin
  working:=true;
  if needResize then begin
     needResize:=false;
     fresize(newWidth, newHeight,newDepth);
     exit;
  end;

  //actually update should be distributed across frames?
  //resume powinno czyscic flagi czy cos bo pauza mogla sie wwybic  polowy updatu ?

  lastTime+=deltaT;
  if lastTime<interval then exit;
  lastTime:=0;

  for z:=0 to depth-1 do
    for x:=0 to width-1 do begin
      if skipUpdate then begin
         working:=false;
         exit;
      end;
    //for i:=0 to length(data)-1 do begin
      //process only world y columns if there's water somwhere on them
      p:=@data[x+(z*world_depth)];
      //p:=it(x,z);
      //p:=@data[i];
      if (p^.skip) then begin
         p^.skip:=false;
         continue;
      end;
      if (p^.wet<255) then begin
         //update interleave
       //  p^.skip:=true;
         y:=p^.wet;
         xx:=x div chunk_width;
         zz:=z div chunk_width;
         a:=x mod chunk_width;
         b:=z mod chunk_width;
         c:=0;
         flag:=true;
         repeat
          blok:=chunksPointer^[xx,zz].pblocks[a,y,b];
          if eBlockTypes(blok^) in substance then inc(c) else begin
             inc(y);
             continue;
          end;
           //now spread to empty blocks nearby and at same level
           { TODO -cwater : watch out for world bounds.. bug on chunk edge}
           //border blocks. calculate only once for each column  pass

           if flag then begin
             xxm:=(x-1) div chunk_width;
             xxp:=(x+1) div chunk_width;
             zzm:=(z-1) div chunk_width;
             zzp:=(z+1) div chunk_width;
             ap:=(x+1) mod chunk_width;
             am:=(x-1) mod chunk_width;
             bp:=(z+1) mod chunk_width;
             bm:=(z-1) mod chunk_width;
             //since loop is randomized we don't know which wet coordinates to use
             wety[0].x:=x-1; wety[0].y:=y; wety[0].z:=z; wety[0].xx:=xxm; wety[0].zz:=zz;
             wety[1].x:=x+1; wety[1].y:=y; wety[1].z:=z; wety[1].xx:=xxp; wety[1].zz:=zz;
             wety[2].x:=x; wety[2].y:=y; wety[2].z:=z-1; wety[2].xx:=xx; wety[2].zz:=zzm;
             wety[3].x:=x; wety[3].y:=y; wety[3].z:=z+1; wety[3].xx:=xx; wety[3].zz:=zzp;
             wety[4].x:=x; wety[4].y:=y-1; wety[4].z:=z; wety[4].xx:=xx; wety[4].zz:=zz;
             wety[5].x:=x; wety[5].y:=y+1; wety[5].z:=z; wety[5].xx:=xx; wety[5].zz:=zz;
             flag:=false;
           end;
           sasiedzi[0]:=chunksPointer^[xxm,zz].pblocks[am,y,b];
           sasiedzi[1]:=chunksPointer^[xxp,zz].pblocks[ap,y,b];
           sasiedzi[2]:=chunksPointer^[xx,zzm].pblocks[a,y,bm];
           sasiedzi[3]:=chunksPointer^[xx,zzp].pblocks[a,y,bp];
           sasiedzi[4]:=chunksPointer^[xx,zz].pblocks[a,y-1,b];
           sasiedzi[5]:=chunksPointer^[xx,zz].pblocks[a,y+1,b];
           j:=XorShift(64);
           for i:=j to j+6 do begin
             wtf:=i mod 6;
             next:=sasiedzi[wtf];

             if (blok^>substanceBase) and (next^=byte(btNone)) then begin
                //loose some value if hitting vacuum
                blok^-=1;
                //blok^:=((blok^-substanceBase) div 2)+substanceBase;
                if blok^>substanceBase then begin
                   next^:=substanceBase;
                   blok^-=1;
                   setWet(wety[wtf].x,wety[wtf].y,wety[wtf].z,true);
                end else blok^:=byte(btNone);
             end
             {
             if (next^=byte(btNone)) then begin
                //loose value if hitting vacuum
                blok^:=byte(btNone) //btWhateverAtmosphereLevelIsOutside
             end
             }
             else
             if (blok^>substanceBase) and
                ((eBlockTypes(next^) in substance) and
                (next^<blok^))  then begin
                   next^+=1;
                   blok^-=1;
                   setWet(wety[wtf].x,wety[wtf].y,wety[wtf].z,true);
                   chunksPointer^[wety[wtf].xx,wety[wtf].zz].reBuild:=true;
                end;
           end;
       inc(y);
       //skip out of loop if reached count of blocks marked for processing
       //or reached world top
       until //(c=0) or
         (y=chunk_height-1) or (skipUpdate);
       if c=0 then p^.wet:=255;
       //terrain.cullBlocksInChunk(chunksPointer,xx,zz,false);
       chunksPointer^[xx,zz].reBuild:=true;
      end;
    end;
end;

procedure TFluidMap.updateGasFillHoles;
var
  a,am,ap,bm,bp,b,x,y,z,c,i,j
  ,xx,zz,xxm,xxp,zzm,zzp,wtf:integer;
  p:pFluidData;
  //:smallint;
  aa,bb,dif,proznia:byte;
  flag:boolean;
  blok,below,next:pBlockType;
  pusteSasiady:integer;
  sasiedzi:array [0..5] of pBlockType;
  wety:array [0..5] of record
    x,z,y,xx,zz:integer;
  end;
begin
  if needResize then begin
     needResize:=false;
     fresize(newWidth, newHeight,newDepth);
     exit;
  end;
  working:=true;
  //actually update should be distributed across frames?
  //resume powinno czyscic flagi czy cos bo pauza mogla sie wwybic  polowy updatu ?

  lastTime+=deltaT;
  if lastTime<interval then exit;
  lastTime:=0;
  proznia:=byte(btNone);
  for z:=0 to depth-1 do
    for x:=0 to width-1 do begin
      if skipUpdate then begin
         working:=false;
         exit;
      end;
    //for i:=0 to length(data)-1 do begin
      //process only world y columns if there's water somwhere on them
      p:=@data[x+(z*world_depth)];
      //p:=it(x,z);
      //p:=@data[i];
      if (p^.skip) then begin
         p^.skip:=false;
         continue;
      end;
      if (p^.wet<255) then begin
         //update interleave
       //  p^.skip:=true;
         y:=p^.wet;
         xx:=x div chunk_width;
         zz:=z div chunk_width;
         a:=x mod chunk_width;
         b:=z mod chunk_width;
         c:=0;
         flag:=true;
         while (y<chunk_height-1) and (not skipUpdate) do begin
          blok:=chunksPointer^[xx,zz].pblocks[a,y,b];
          if eBlockTypes(blok^) in substance then inc(c) else begin
             inc(y);
             continue;
          end;
           //now spread to empty blocks nearby and at same level
           { TODO -cwater : watch out for world bounds.. bug on chunk edge}
           //border blocks. calculate only once for each column  pass

           if flag then begin
             xxm:=(x-1) div chunk_width;
             xxp:=(x+1) div chunk_width;
             zzm:=(z-1) div chunk_width;
             zzp:=(z+1) div chunk_width;
             ap:=(x+1) mod chunk_width;
             am:=(x-1) mod chunk_width;
             bp:=(z+1) mod chunk_width;
             bm:=(z-1) mod chunk_width;
             //since loop is randomized we don't know which wet coordinates to use
             wety[0].x:=x-1; wety[0].y:=y; wety[0].z:=z; wety[0].xx:=xxm; wety[0].zz:=zz;
             wety[1].x:=x+1; wety[1].y:=y; wety[1].z:=z; wety[1].xx:=xxp; wety[1].zz:=zz;
             wety[2].x:=x; wety[2].y:=y; wety[2].z:=z-1; wety[2].xx:=xx; wety[2].zz:=zzm;
             wety[3].x:=x; wety[3].y:=y; wety[3].z:=z+1; wety[3].xx:=xx; wety[3].zz:=zzp;
             wety[4].x:=x; wety[4].y:=y-1; wety[4].z:=z; wety[4].xx:=xx; wety[4].zz:=zz;
             wety[5].x:=x; wety[5].y:=y+1; wety[5].z:=z; wety[5].xx:=xx; wety[5].zz:=zz;
             flag:=false;
           end;
           sasiedzi[0]:=chunksPointer^[xxm,zz].pblocks[am,y,b];
           sasiedzi[1]:=chunksPointer^[xxp,zz].pblocks[ap,y,b];
           sasiedzi[2]:=chunksPointer^[xx,zzm].pblocks[a,y,bm];
           sasiedzi[3]:=chunksPointer^[xx,zzp].pblocks[a,y,bp];
           sasiedzi[4]:=chunksPointer^[xx,zz].pblocks[a,y-1,b];
           sasiedzi[5]:=chunksPointer^[xx,zz].pblocks[a,y+1,b];
           j:=XorShift(7);
           { TODO -copti : use faster random generator }
           //count empty blocks around
           //pusteSasiady:=0;
           //for i:=0 to 5 do if sasiedzi[i]^=byte(btNone) then inc(pusteSasiady);
           //annihilate thinest oxygen touching some pusteSasiady
           //if pusteSasiady>1 then if blok^=substanceBase then blok^:=byte(btNone);
          // if blok^<>byte(btNone) then

          //first try go up to the sky
           if sasiedzi[5]^=proznia then begin
              sasiedzi[5]^:=blok^;
              blok^:=proznia;
              setWet(wety[5].x,wety[5].y,wety[5].z,true);
              if not (btOxy1 in mineralsDontRender) then
                 //terrain.cullBlocksInChunk(chunksPointer,wety[5].xx,wety[5].zz,false);
                 chunksPointer^[wety[5].xx,wety[5].zz].reBuild:=true;
           end else
           for i:=j to j+6 do begin
             wtf:=i mod 6;
             next:=sasiedzi[wtf];
             //tlen jest nieporzadany jak goracy kartofel. jezeli jest puste miejsce obok
             //to sie go tam wciska
             if (next^=proznia) and (blok^=substanceBase)  then begin
                //teleport
                next^:=blok^;
                blok^:=proznia;
                setWet(wety[wtf].x,wety[wtf].y,wety[wtf].z,false);
                if not (btOxy1 in mineralsDontRender) then
                   chunksPointer^[wety[wtf].xx,wety[wtf].zz].reBuild:=true;
                   //terrain.cullBlocksInChunk(chunksPointer,wety[wtf].xx,wety[wtf].zz,false);
                break;
             end else
             //if some bigger block encounters proznia they both turn to thin air
             if (next^=proznia) and (blok^>substanceBase) then begin
                blok^:=substanceBase;
                next^:=substanceBase;
                setWet(wety[wtf].x,wety[wtf].y,wety[wtf].z,false);
                if not (btOxy1 in mineralsDontRender) then
                   chunksPointer^[wety[wtf].xx,wety[wtf].zz].reBuild:=true;
                   //terrain.cullBlocksInChunk(chunksPointer,wety[wtf].xx,wety[wtf].zz,false);
               // break;
             end
             //if air meets air then switch places
             else
             if (eblockTypes(next^) in substance) then begin
                if blok^>next^ then begin
                   dif:=(blok^-next^)div 2;
                   if dif=0 then dif:=1;
                   blok^-=dif;
                   next^+=dif;
                end else
                if blok^<next^ then begin
                   dif:=(next^-blok^) div 2;
                   if dif=0 then dif:=1;
                   blok^+=dif;
                   next^-=dif;
                end;
                setWet(wety[wtf].x,wety[wtf].y,wety[wtf].z,false);
                if not (btOxy1 in mineralsDontRender) then
                   chunksPointer^[wety[wtf].xx,wety[wtf].zz].reBuild:=true;
                   //terrain.cullBlocksInChunk(chunksPointer,wety[wtf].xx,wety[wtf].zz,false);
               // break;
             end;
           end;
       inc(y,1);
       //skip out of loop if reached count of blocks marked for processing
       //or reached world top
       end; //(c=0) or
       if c=0 then p^.wet:=255;
       if not (btOxy1 in mineralsDontRender) then
          chunksPointer^[xx,zz].reBuild:=true;
          //terrain.cullBlocksInChunk(chunksPointer,xx,zz,false);
      end;
    end;
end;

procedure TFluidMap.leresize(w, h, d: integer);
begin
  newWidth:=w;
  newHeight:=h;
  newDepth:=d;
  needResize:=true;
end;

procedure TFluidMap.fresize(w, h, d: integer);
begin
  width:=w;
  height:=h;
  depth:=d;
  setlength(data,w*d);
  for w:=low(data) to high(data) do begin
    data[w].wet:=255;
    //data[w].count:=0;
  end;
end;

procedure TFluidMap.setWet(x, y, z: cardinal; alsoSkip: boolean=false);
var b:pFluidData;
begin
  //check bounds?
  b:=it(x,z);
  if b^.wet=255 then b^.wet:=y else
     if b^.wet>y then b^.wet:=y;
  if alsoSkip then b^.skip:=true;
end;

function TFluidMap.it(const x, z: cardinal): pFluidData;
begin
  result:=@data[x + (z * world_depth)];
end;

constructor TFluidMap.create(w, h, d: integer; typ: eSubstanceTypes;
  updateInterval: single);
begin
  case typ of
    stWater: begin
      substance:=mineralsWater;
      substanceBase:=byte(btWater1);
      substanceMax:=byte(btWater7);
    end;
    stOxygen:begin
     substance:=mineralsOxygen;
     substanceBase:=byte(btOxy1);
     substanceMax:=byte(btOxy10);
    end;
  end;
  leresize(w,h,d);
  interval:=updateInterval;
end;


{ TAutomataGenerator }

procedure TAutomataGenerator.setPower(AValue: integer);
begin
  if btWater1 in substance then fpower:=eBlockTypes(byte(btWater1)+avalue-1)
  else if btOxy1 in substance then fpower:=eBlockTypes(byte(btOxy1)+avalue-1);
end;

function TAutomataGenerator.getBlockInWorld(x, y, z: integer; centered: boolean
  ): pBlockType;
var
    chX,chY:integer;
    blokPos:TVector3b;
    bloke:eBlockTypes;
begin
  if centered then begin
    chX:=round(x+world_width2) div( chunk_width);
    chY:=round(z+world_depth2) div ( chunk_width);
    //if chX>=active_chunks_w then chX:=active_chunks_w-1;
    //if chY>=active_chunks_h then chY:=active_chunks_h-1;
    blokPos:=vector3bmake(abs(round(x+world_width2)) mod (chunk_width),
           round(y),
           abs(round(z+world_depth2)) mod (chunk_width));
    result:=chunksPointer^[chx,chy].pblocks[blokPos[0],blokPos[1],blokPos[2]];
  end
  else begin
    result:=chunksPointer^[x div chunk_width,z div chunk_width].pblocks[x mod chunk_width,y,z mod chunk_width];
  end;
end;

procedure TAutomataGenerator.setBlockInWorld(x, y, z: integer;blok: eBlockTypes; centered,cull: boolean);
var chX,chY:integer;
    blokPos:TVector3b;
    bloke:eBlockTypes;
begin
  if centered then begin
    chX:=round(x+world_width2) div( chunk_width);
    chY:=round(z+world_depth2) div ( chunk_width);
    if chX>=active_chunks_w then chX:=active_chunks_w-1;
    if chY>=active_chunks_h then chY:=active_chunks_h-1;
    blokPos:=vector3bmake(abs(round(x+world_width2)) mod (chunk_width),
           round(chunk_height+y),
           abs(round(z+world_depth2)) mod (chunk_width));
    blok:=eBlockTypes(chunksPointer^[chx,chy].blocks[blokPos[0],blokPos[1],blokPos[2]]);
    chunksPointer^[chx,chy].setBlock(blokPos,blok);
    if cull then chunksPointer^[chx,chy].reBuild:=true;
    // cullBlocksInChunk(chunksPointer,chX,chY,false);
    //blok:=btNone;
  end else begin
    chunksPointer^[x div chunk_width,z div chunk_width].blocks[x mod chunk_width,y,z mod chunk_width]:=byte(blok);
  end;
end;

function TAutomataGenerator.getPower: integer;
begin
  result:=byte(fpower)-byte(low(substance));
end;

procedure TAutomataGenerator.update;
var i:integer;
    blok:pBlockType;
begin
  lastUpdate+=deltaT;
  map^.setWet(position[0],position[1],position[2]);
{ TODO -ctodo : niech istnieje tez tryb w ktorym woda sie nie przelewa ponad poziom }
  if lastUpdate>interval then begin
     //water generator
     if btWater1 in substance then begin
       //if 'emmiter' block already full then pump up
       //najpierw wypadalo by sprawdzic czy wszyscy sasiedzi sa pelni zanim zrobimy gejzer
       if (getBlockInWorld(position[0],position[1],position[2],false)^=substanceMax)
       //and (getBlockTypeAt(vector3fmake(position[0],position[1],position[2])=substanceMax)
       then begin
        for i:=position[1]+1 to chunk_height-1 do begin
           blok:=getBlockInWorld(position[0],position[1],position[2],false);
           //jezeli pusty to fill him
           if blok^=byte(btNone) then begin
              blok^:=substanceMax;
              break;
           end
           else
              //jezeli matroche wody to
              if eblockTypes(blok^) in substance then begin
                  //jezeli fpower<substanceMax to moze cos dziwnie dzialac
                 setBlockInWorld(position[0],i,position[2],eblockTypes(blok^),false,true);
                 map^.setWet(position[0],position[1],position[2]);
                 //blok^:=byte(fPower);
                 break;
              end
           //break on obstacle. this means that source can be blocked by placing block
           //in water above it..
           else break;
        end;
       end else
         setBlockInWorld(position[0],position[1],position[2],fpower,false,true);
     end
     //oxygen generator
     else begin
       blok:=getBlockInWorld(position[0],position[1],position[2],false);
      { if blok^=byte(btNone) then
         terrain.setBlockInWorld(position[0],position[1],position[2],eBlockTypes(substanceBase),false,true)
       else
       if blok^<substanceMax then
         terrain.setBlockInWorld(position[0],position[1],position[2],eBlockTypes(blok^+1),false,false);
         }
       setBlockInWorld(position[0],position[1],position[2],eblocktypes(substanceMax),false,false);
     end;
     lastUpdate:=0;
     //atmosphere.skipUpdate:=false;
  end;
end;

constructor TAutomataGenerator.create(pos: TVector3i; fluidmap:pFluidMap; aPower:integer;
  timeStep: single);
begin
  position:=pos;
  map:=fluidMap;
  substance:=map^.substance;
  setPower(apower);
  interval:=timeStep;
  substanceBase:=byte(mineralFromSet(substance,0));
  substanceMax:=byte(mineralFromSet(substance,getSetSize(substance)-1));
end;

constructor TAutomataGenerator.create(pos: TAffineVector; fluidmap:pFluidMap;
  aPower:integer; timeStep: single);
begin
  create(vector3imake(round(pos[0]),round(pos[1]),round(pos[2])),fluidmap,
  aPower,TimeStep);
end;

end.

