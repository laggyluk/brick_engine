unit core_terrain_generator;

{$mode objfpc}{$H+}
//This is required with Lazarus on x86:

interface

uses
  Classes, SysUtils,graphics,core_types,core_chunk,TypInfo,inifiles,
  core_utils,VectorTypes,VectorGeometry,core_classes,perlinnoise,
  core_noise,  core_camera, core_atmosphere//,core_main
  {$IFNDEF SERVER},core_lights,core_block_brush{$ENDIF};

//const aoStep = 0.05;

type
  //8 block neighbours
  eSasiedzi = (sN,sNE,sE,sSE,sS,sSW,sW,sNW);
  TSasiedzi = set of eSasiedzi;
  pSasiedzi = ^TSasiedzi;
  eFaces = (fTop,fBottom,fLeft,fRight,fFront,fBack);
  //used to inform core about new map size in MIDDLE of loading it
  TMapSetSizeCallback = procedure (chunkW,chunkH,mapW,mapH:integer) of object;

  { TTerrainGenerator }

  TTerrainGenerator = class (TUpdateable)
      bmpBuffer : tbitmap;
      lastOscColour:tvector3f;//last colour value from oscilator
      //used by chunk culling in round robin
      lastCulledChunk:integer;
      oscUp:boolean;//does oscilator tint rise or fall?
      //set to last chunk passed to cullBlocksInChunk. used by perlin color oscilator
      currentChunkOff:tvector2i;
    private
      cullChunksQueue: array of array of boolean;
      fileHeaderSize:integer; //size of header info, after this world data starts
      vboDumpCount:cardinal;
      //ambient occlusion values
      procedure aoPass(chunk: pChunk);
    public
      //will be stored in header. probably
      maxPlayers:byte;
      description:string;
      mapFileName:string;
      mapSizeCallback:TMapSetSizeCallback;
      //map size in blocks
      mapSize:TAffinevector;
      constructor create;
      destructor destroy; override;
      procedure setMapSize;
      procedure perlin(w,h:integer);
      procedure loadFromImage(FileName:string);
      procedure freeBufferBmp;
      //compresses & saves given sector of active_chunk_w*h size to a matching file
      procedure saveSector;
      procedure loadBlockColors(fiile: string);
      procedure saveBlockColors(fiile: string);
      procedure CpyFromBmpBufferToChunkBuffer(blocks: PBlocksBuffer;chunkX,chunkZ:integer);
      //generates optimized mesh by merging neigbourhing cube sides
      procedure cullBlocksInChunk(chunks:PChunksArray;chunkX,chunkY:integer;executeNow:boolean=false);
      procedure cullAllChunks;
      //calculates light strength at given face
      function calculateAO(xx, yy, zz: integer; dir: eFaces): single;
      //set block and cull chunk. takes coordinates starting at 0,0 center
      procedure setBlockAndCull(pos:TAffineVector;blokType:eBlockTypes);
      //set blok in world coordinates either starting at world 0,0 or at array 0,0
      procedure setBlockInWorld(x, y, z: integer; blok: eBlockTypes; centered,
        cull: boolean);
      function getBlockInWorld(x, y, z: integer;centered:boolean):pBlockType;
      procedure saveMap(filename:string);
      procedure loadMap(filename:string);
      //fills map with a big cross
      procedure initMap(empty: boolean);
      procedure generateMap(flatness: single; cutoff: integer; xyScale: single;
        hangover: single; mineral1, mineral2, mineralBottom,
  mineralBottom2: eBlockTypes);
      //color tint acros terrain can change back and forth
      procedure colorOscilator(var r,g,b:single);
      //color variation based on pre generated perlin map
      function perlinOscilator(const x,y,z:single):single;
      {$IFNDEF SERVER}
      //saves whole map as brush file
      procedure saveMapAsBrush(const filename:string);
      {$ENDIF}
      //global iterator service
      procedure ForEachBlockInChunk(chunk: pChunk; const operation: TBlockMutator;
        cullAfter: boolean);
      procedure ForEachBlockInWorld(const operation: TBlockMutator;
        optimize: boolean; cullAfter: boolean);
      //convert form 'minus' coordinates to array
      function fromWorldToArray(pos:TAffineVector):TVector3i;
      //match block mutator signature
      procedure MutatorBlockAssignValue(block: PBlockType);
      procedure MutatorZeroBlocks(block: PBlockType);
      //processes queued tasks like chunks culling
      procedure Update;override;
      //saves effect of chunk last culling to file
      procedure saveVboBuffToFile(const filename: string);
  end;

  //set block at absolute position
  procedure setBlockAt(pos:TAffineVector;blok:eblocktypes);inline;
  procedure setBlockAt(pos:TVector3i;blok:eblocktypes;cullChunk:boolean);inline;
  procedure setBlockAtFast(x,y,z:integer;blok:eblocktypes;cullChunk:boolean);inline;

var terrain:TTerrainGenerator;
    //color change across x-z plane
    colorScilatorTint:single = 0.0001;
    //how fast color changes:
    colorScilatorStep:single = 0.000002;
    //color lighten with height. block.color+=block.y / heightTint
    heightTint:single = 500;

implementation

procedure setBlockAtFast(x, y, z: integer; blok: eblocktypes; cullChunk: boolean);
var xx,yy:integer;
begin
  xx:=x div chunk_width;
  yy:=y div chunk_width;
  chunksPointer^[xx,yy].blocks[x mod chunk_width,y,z mod chunk_width]:=byte(blok);
  //chunksPointer^[xx,yy].blocks[x mod chunk_width,y,z mod chunk_width]:=byte(blok);
  if cullChunk then terrain.cullBlocksInChunk(chunksPointer,xx,yy,false);
end;

procedure setBlockAt(pos: TAffineVector; blok: eblocktypes);
var
  ch:TVector2i;
  blokpos:TVector3b;
begin
  //chances are this is fucked up
  blokpos:=absolutePositionToChunkBlok(ch,pos);
  if (ch[0]<active_chunks_w) and (ch[1]<active_chunks_h) and (blokpos[1]<chunk_height)
  then begin
    chunksPointer^[ch[0],ch[1]].blocks[blokPos[0],blokPos[1],blokPos[2]]:=byte(blok);
    terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1],false);
//    chunksPointer^[ch[0],ch[1]].blocks[blokPos[0],blokPos[1],blokPos[2]].
  end
  else
    log('setBlockAt:: out of bounds');
end;

procedure setBlockAt(pos: TVector3i; blok: eblocktypes;cullChunk:boolean);
var x,y:integer;
begin
    //chances are this is fucked up
  if pos[1]<0 then pos[1]:=chunk_height+pos[1];
  x:=pos[0] div chunk_width;
  y:=pos[2] div chunk_width;
  chunksPointer^[x,y].blocks[pos[0] mod chunk_width,pos[1],pos[2] mod chunk_width]:=byte(blok);
  if cullChunk then terrain.cullBlocksInChunk(chunksPointer,x,y,false);
end;

var  fileBuf:array of TBlockType;
     bufPosition:cardinal;//current position in chunksBuffer
     topFaces,frontFaces,backFaces : array of eBlockTypes;


//<<Noise Routines
procedure TTerrainGenerator.saveMap(filename:string);
var
  x,y,z,w,h:integer;
  fileStream:TFilestream;
begin

 try
  if filename='' then begin
      log('achtung! saveMap::no file specified');
     exit;
  end;
  //compressed map?
  if ExtractFileExt(filename)='.lemap' then begin
     exit;
  end;
  if atmosphere<>nil then atmosphere.pause;
  fileStream:=tfilestream.Create(filename,fmOpenWrite or fmCreate);
  mapFileName:=filename;
  filestream.Seek(0,soFromBeginning);
  //map dimensions
  fileStream.Write(active_chunks_w,sizeof(active_chunks_w));
  fileStream.Write(active_chunks_h,sizeof(active_chunks_h));
  //chunk dimensions
  fileStream.Write(chunk_width,sizeof(chunk_width));
  fileStream.Write(chunk_height,sizeof(chunk_height));
  //save chunks data
  if length(filebuf)<>chunk_height then
     setlength(filebuf,chunk_height);
  for w:=0 to active_chunks_h-1 do
    for h:=0 to active_chunks_w-1 do
       for z:=0 to chunk_width-1 do
          for x:=0 to chunk_width-1 do begin
             for y:=0 to chunk_height-1 do begin
                filebuf[y]:= chunksPointer^[w,h].pblocks[x,y,z]^;
                //filebuf[y]:=core.chunks[w,h].blocks[x,y,z];
             end;
             filestream.WriteBuffer(filebuf[0],length(filebuf));
          end;
  filestream.free;
  {$IFNDEF SERVER}
  //save lights. should be moved to renderer to remove dependency
  {
  filename:=ChangeFileExt(filename,'.lights');
  fileStream:=tfilestream.Create(filename,fmOpenWrite or fmCreate);
  //directional
  fileStream.Write(renderer.dirLight.color,sizeof(renderer.dirLight.color));
  fileStream.Write(renderer.dirLight.direction,sizeof(renderer.dirLight.direction));
  fileStream.Write(renderer.dirLight.ambientIntensity,sizeof(renderer.dirLight.ambientIntensity));
  fileStream.Write(renderer.dirLight.diffuseIntensity ,sizeof(renderer.dirLight.diffuseIntensity));
  //point lights
  fileStream.Write(renderer.getLevelLightsCount,sizeof(integer));
  for i:=0 to renderer.pointLightsDS.Count-1 do if renderer.pointLightsDS[i].staticLight then
  with renderer.pointLightsDS[i] do begin
    fileStream.Write(color,sizeof(color));
    fileStream.Write(position,sizeof(position));
    fileStream.Write(ambientIntensity,sizeof(ambientIntensity));
    fileStream.Write(diffuseIntensity,sizeof(diffuseIntensity));
    fileStream.Write(attenuation,sizeof(attenuation));
  end;
  filestream.Free;}
  {$ELSE}
  log('achtung! map lights are not saved with map data in server mode');
  {$ENDIF}
  //save palette
  filename:=ChangeFileExt(filename,'.palette');
  saveBlockColors(filename);
  finally

   if atmosphere<>nil then atmosphere.resume;
  end;
end;

procedure TTerrainGenerator.loadMap(filename:string);
var
  x,y,z,w,h:integer;
  c:longint;
  fileStream:TFilestream;
  position,color:TAffineVector;
  ambientIntensity,diffuseIntensity:single;
  {$IFNDEF SERVER}
  attenuation:rAttenuation;
  l:tpointlight;
  {$ENDIF}
begin
 try
  //compressed map?
  if ExtractFileExt(filename)='.lemap' then begin
     exit;
  end;
  //om.addOrder(orMapLoad,0,0);
  if atmosphere<>nil then atmosphere.pause;
  fileStream:=tfilestream.Create(filename,fmOpenRead);
  mapFileName:=filename;
  filestream.Seek(0,soFromBeginning);
  //map dimensions
  fileStream.Read(w,sizeof(w));
  fileStream.Read(h,sizeof(h));
  //chunk dimensions
  fileStream.Read(x,sizeof(x));
  fileStream.Read(y,sizeof(y));
  setVector(mapSize,w*x,h*x,y);
  //core.setMapSize(x,y,w,h);
  mapSizeCallback(x,y,w,h);
  setmapsize;
  if atmosphere<>nil then atmosphere.resize(w*x,y,h*x);


  //fill them with something more usefull;
  if length(filebuf)<>chunk_height then
     setlength(filebuf,chunk_height);

   for w:=0 to active_chunks_h-1 do
     for h:=0 to active_chunks_w-1 do
       for z:=0 to chunk_width-1 do
          for x:=0 to chunk_width-1 do begin
           filestream.ReadBuffer(filebuf[0],length(filebuf));
           //copy(core.chunks[w,h].pblocks[x,0,z]^ ,0,chunk_height);
           for y:=0 to chunk_height-1 do  begin
               //core.chunks[w,h].blocks[x,y,z]:=filebuf[y];
              chunksPointer^[w,h].pBlocks[x,y,z]^:=filebuf[y];
           end;
          end;
    for x:=0 to active_chunks_h-1 do
      for y:=0 to active_chunks_w-1 do
        cullBlocksInChunk(chunksPointer,y,x);

    filestream.free;
    {$IFNDEF SERVER}
    //load ligths-- should be moved to renderer
   {
    renderer.removeLights;
    filename:=ChangeFileExt(filename,'.lights');
    fileStream:=tfilestream.Create(filename,fmOpenReadWrite);

    //load dir light
    fileStream.Read(renderer.dirLight.color,sizeof(renderer.dirLight.color));
    fileStream.Read(renderer.dirLight.direction,sizeof(renderer.dirLight.direction));
    fileStream.read(renderer.dirLight.ambientIntensity,sizeof(renderer.dirLight.ambientIntensity));
    fileStream.read(renderer.dirLight.diffuseIntensity ,sizeof(renderer.dirLight.diffuseIntensity));
    //read number of pointlights
    fileStream.read(y,sizeof(y));
    //load point lights
    for x:=0 to y-1 do begin
      fileStream.Read(color,sizeof(color));
      fileStream.Read(position,sizeof(position));
      fileStream.Read(ambientIntensity,sizeof(ambientIntensity));
      fileStream.Read(diffuseIntensity,sizeof(diffuseIntensity));
      fileStream.Read(attenuation,sizeof(attenuation));
      l:=tpointlight.create(color,position,diffuseIntensity,attenuation.constant,
          attenuation.linear,attenuation.exp);
      renderer.addPointLight(l);
    end;
    filestream.Free;}
    {$ELSE}
    log('achtung! map lights are not loaded in server mode');
    {$ENDIF}
    //load palette
    filename:=ChangeFileExt(filename,'.palette');
    loadBlockColors(filename);
 finally
   showLoadingScreen:=false;
   if atmosphere<>nil then atmosphere.resume;
 end;
end;

procedure TTerrainGenerator.initMap(empty:boolean);
var
  chx,chy,x,y,z,i,j,xx,yy,zz:integer;
  pBlok:pBlockType;
begin
  if atmosphere<>nil then atmosphere.pause;
  setMapSize;
  if atmosphere<>nil then atmosphere.resize(world_width,chunk_height,world_depth);
  ForEachBlockInWorld(@MutatorZeroBlocks,true,false);
  y:=chunk_height div 4;
   for z:=0 to world_depth-1 do begin
     for x:=0 to world_width-1 do  begin
      chx:=x div chunk_width;
      chy:=z div chunk_width;
      xx:=x mod chunk_width;
      yy:=y;
      zz:=z mod chunk_width;
      pBlok:=chunksPointer^[chx,chy].pblocks[xx,yy,zz];
      pBlok^:=byte(btStone);
      if (x=z) or (z=world_width-x) then pBlok^:=byte(btRedRock);
      if empty then if (x<-1+world_width2) or (x>1+world_width2)
         or (z<-1+world_width2) or (z>1+world_width2) then pBlok^:=byte(btNone);
    end;
   end;
   for z:=0 to active_chunks_h-1 do
     for x:=0 to active_chunks_w-1 do
       cullBlocksInChunk(chunksPointer,x,z);
//   loadBlockColors(appPath+'terrain\blockColorDefs.palette');
   camera.setTarget(0,chunk_height2 div 2,0);
   camera.setOrigin(0,chunk_height,80);

   if atmosphere<>nil then atmosphere.resume;
end;

procedure TTerrainGenerator.generateMap(flatness: single; cutoff: integer;
          xyScale:single;hangover:single;mineral1,mineral2,mineralBottom,mineralBottom2:eBlockTypes);
var
  chx,chy,x,y,z,xx,yy,zz:integer;
  //big masses. from bottom up
  scale,noise,noise2:single;
  t1,t2:single;
  pBlok:pBlockType;
begin
 try
  if atmosphere<>nil then  atmosphere.pause;

  t1:=1.1;
  for y:=0 to chunk_height-1 do begin
   // log(floattostr(t1)+' < '+floattostr(noise));
    for z:=0 to world_depth-1 do begin
      for x:=0 to world_width-1 do  begin
       chx:=x div chunk_width;
       chy:=z div chunk_width;
       xx:=x mod chunk_width;
       yy:=y;
       zz:=z mod chunk_width;
       pBlok:=chunksPointer^[chx,chy].pblocks[xx,yy,zz];
       //zero the block. will get filled in next step and then will be used in boolean cut
       pBlok^:=byte(btNone);
       scale:=10;
        //underground
      if
        // (chy<active_chunks_h-1) and
         (y<chunk_height2) then begin
//      (btIron,mineralBottom,btQuartzite,mineral1)
        //bottomest layer
        t1:=(y/cutoff*0.234);
        noise:=simplex_Noise(1,x/scale,y/scale*2,z/scale);
        //simplex_Noise(1,x/scale,y/scale*1.5,z/scale);
        if (t1>noise) then chunksPointer^[chx,chy].setBlock(xx,yy,zz,mineralBottom)
           else chunksPointer^[chx,chy].setBlock(xx,yy,zz,mineralBottom2);//bottom
        t2:=(y/cutoff*0.39);
        noise:=simplex_Noise(1,x/scale,y/scale*2,z/scale);
        if (t2>noise) then pBlok^:=byte(mineralBottom2);
        t2:=(y/(cutoff*0.625));
        noise:=simplex_Noise(1,x/(2*scale),y/(4*scale),z/(2*scale));
        if (t2>noise) then pBlok^:=byte(mineral1);
      end;
    //near surface
    if
       //(chy<active_chunks_h-1) and
       (y>cutoff*0.75) then begin
        t2:=(y/cutoff);
        noise:=simplex_Noise(1,x/(1.5*scale),y/(4*scale),z/(1.5*scale));
//        if (t2<noise) then if pblok^=btNone then pBlok^:=btRedSlab;
        if (t2>noise) then pBlok^:=byte(mineral2) else pBlok^:=byte(mineral1);
    end;
    end;
   end;
  end;
  //add some perlin mountains. this time iterating up each column
  for z:=0 to world_depth-1 do begin
    //log(floattostr(noise));
    for x:=0 to world_width-1 do
      for y:=0 to chunk_height-1 do begin
       chx:=x div chunk_width;
       chy:=z div chunk_width;
       xx:=x mod chunk_width;
       yy:=y;
       zz:=z mod chunk_width;
       pBlok:=chunksPointer^[chx,chy].pblocks[xx,yy,zz];
       noise:=-perlin3DNoise.noise(x/512*4*xyScale,z/512*4*xyScale)*flatness+cutoff;
       noise2:=-perlin3DNoise.noise(x/256*4*xyScale,y/256*6*xyScale,z/256*4*xyScale)*hangover*5;
       if y>noise+noise2 then pBlok^:=byte(btNone);// else pBlok^:=mineral1;
       //add poziome distortion to surface
       if y>cutoff then begin
          //noise:=-perlin3DNoise.noise(x/512*4*xyScale,y/512*4*xyScale,z/512*4*xyScale)*hangover+cutoff;
          //if y>noise then pBlok^:=btNone;
       end;
      end;
  end;

  //cull blocks
  for z:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do
      cullBlocksInChunk(chunksPointer,x,z);
  finally
  if atmosphere<>nil then   atmosphere.resume;
 end;
end;

procedure TTerrainGenerator.colorOscilator(var r, g, b: single);
begin
  //perlin3DNoise.Noise();
  if oscUp then begin
     r:=r+colorScilatorTint;
     g:=g+colorScilatorTint;
     b:=b+colorScilatorTint;
     colorScilatorTint:=colorScilatorTint+colorScilatorStep;
     if colorScilatorTint>0.001 then oscUp:=false;
  end else begin
     r:=r-colorScilatorTint;
     g:=g-colorScilatorTint;
     b:=b-colorScilatorTint;
     colorScilatorTint:=colorScilatorTint-colorScilatorStep;
    if colorScilatorTint<0.001 then oscUp:=true;
  end;
end;

function fmod(x,y:single):single;
begin
 result:= x - int(x/y) * y;
end;

function TTerrainGenerator.perlinOscilator(const x, y, z: single): single;
begin
//  currentChunkOff[0]*chunk
  if bmpBuffer=nil then
      perlin(world_width,world_depth);
  { TODO -copti : guess 2d byte array could be faster to access than canvas pixels }
  result:=(bmpBuffer.Canvas.Pixels[round(x+currentChunkOff[0]),round(z+currentChunkOff[1])] mod $ff);
//  result:=perlin3DNoise.Noise(fmod(x+fmod(currentChunkOff[0],60),1),y/chunk_height,fmod(z+currentChunkOff[1],60));
end;

{$IFNDEF SERVER}
procedure TTerrainGenerator.saveMapAsBrush(const filename: string);
var
  x,y,z,chx,chy,xx,yy,zz:integer;
  brush:TBlockBrush;
  pBlok:pBlockType;
begin
 if atmosphere<>nil then  atmosphere.pause;
 brush:=TBlockBrush.create;
 for z:=0 to world_depth-1 do begin
   chy:=z div chunk_width;
   zz:=z mod chunk_width;
   for x:=0 to world_width-1 do begin
     chx:=x div chunk_width;
     xx:=x mod chunk_width;
     for y:=0 to chunk_height-1 do begin
      yy:=y;
      zz:=z mod chunk_width;
      pBlok:=chunksPointer^[chx,chy].pblocks[xx,yy,zz];
      if pblok^<>byte(btNone) then  brush.addBlok(vector3fmake(x,y,z),eBlockTypes(pblok^));
     end;
   end;
 end;

 brush.saveAs(filename);
 brush.destroy;
 if atmosphere<>nil then atmosphere.resume;
end;
{$ENDIF}

procedure TTerrainGenerator.ForEachBlockInChunk(chunk: pChunk;
  const operation: TBlockMutator; cullAfter:boolean);
var
  pBlok: pBlockType;
  x: Integer;
  y: Integer;
  z: Integer;
begin
  //for each block do operation, 3loops, 26ops

  for x:=0 to chunk_width-1 do
      for y:=0 to chunk_height-1 do
          for z:=0 to chunk_width-1 do begin
              pBlok := Chunk^.pblocks[x, y, z];
              operation(pBlok);
          end;

  if cullAfter then cullBlocksInChunk(chunksPointer,chunk^.xID,chunk^.yID,false);
end;

procedure TTerrainGenerator.ForEachBlockInWorld(const operation: TBlockMutator;
  optimize:boolean;cullAfter: boolean);
var
  pBlok: pBlockType;
  x: Integer;
  y: Integer;
  z: Integer;
  chx,chy,zz,xx: Integer;
begin
 //process blocks chunk by chunk. should be faster.
 if optimize then begin
  for chY:=0 to active_chunks_h-1 do
    for chX:=0 to active_chunks_w-1 do
      for y:=0 to chunk_height-1 do
        for z:=0 to chunk_width-1 do
          for x:=0 to chunk_width-1 do begin
            pBlok := chunksPointer^[chx,chy].pblocks[x, y, z];
            operation(pBlok);
          end;
 end
 else begin
  //process world row by row
  for y:=0 to chunk_height-1 do begin
     chx:=x div chunk_width;
     chy:=z div chunk_width;
     for z:=0 to world_depth-1 do begin
       zz:=z mod chunk_width;
       for x:=0 to world_width-1 do begin
        xx:=x mod chunk_width;
        pBlok:=chunksPointer^[chx,chy].pblocks[xx,y,zz];
        operation(pBlok);
       end;
     end;
   end;
 end;
end;

function TTerrainGenerator.fromWorldToArray(pos: TAffineVector): TVector3i;
var
  chX,chY:integer;
begin
    chX:=round(pos[0]+world_width2) div( chunk_width);
    chY:=round(pos[2]+world_depth2) div ( chunk_width);
    //log an error?
    if chX>=active_chunks_w then chX:=active_chunks_w-1;
    if chY>=active_chunks_h then chY:=active_chunks_h-1;
    result:=vector3imake(abs(round(pos[0]+world_width2)) mod (chunk_width),
           round(chunk_height+pos[1]),
           abs(round(pos[2]+world_depth2)) mod (chunk_width));
end;

procedure TTerrainGenerator.MutatorBlockAssignValue(block: PBlockType);
begin
   block^:=byte(btwater1);
end;

procedure TTerrainGenerator.MutatorZeroBlocks(block: PBlockType);
begin
   block^:=byte(btNone);
end;

procedure TTerrainGenerator.Update;
var
  x,y:integer;
begin
  //atmosphere.update;
  //cull blocks in one chunk per frame if queued
  x:=lastCulledChunk mod active_chunks_w;
  y:=lastCulledChunk div active_chunks_h;
  if (cullChunksQueue[x,y]) or (chunksPointer^[X,Y].reBuild) then cullBlocksInChunk(chunksPointer,x,y,true);
  inc(lastCulledChunk);
  if lastCulledChunk>=active_chunks_w*active_chunks_w then lastCulledChunk:=0;
end;

procedure TTerrainGenerator.saveVboBuffToFile(const filename: string);
var
  st:tfilestream;
  i,verts:cardinal;
  b:rBufferDataBit;
  xMin,xMax,zMin,zMax,yMin,yMax,xMid,zMid:single;
begin
 try
  xMin:=MaxSingle;xMax:=MinSingle;zMin:=MaxSingle;zmax:=MinSingle;zMid:=MinSingle;yMin:=MaxSingle;yMax:=MinSingle;
  //first find the 'pivot' at the center of bottom
  verts:=(vboDumpCount div sizeof(rBufferDataBit))-1;
  for i:=0 to verts do begin
    b:=chunkVertexBuffer[i];
    b.x+=world_width2;
    b.z+=world_depth2;
    if b.x<xmin then xMin:=b.x;
    if b.x>xMax then xMax:=b.x;
    if b.z<zmin then zMin:=b.z;
    if b.z>zMax then zMax:=b.z;
    if b.y<yMin then yMin:=b.y;
    if b.y>yMax then ymax:=b.y;
    chunkVertexBuffer[i]:=b;
  end;
  //then substract it from rest of blocks to 'center' them
  xMid:=(xMax-xMin) / 2;
  zMid:=(zMax-zMin) / 2;
  for i:=0 to verts do begin
    b:=chunkVertexBuffer[i];
    b.x-=xMin;
    b.x-=xMid;
    b.z-=zMin;
    b.z-=zMid;
    b.y-=yMin;
    chunkVertexBuffer[i]:=b;
  end;
  st:=tFilestream.create(filename,fmCreate or fmOpenWrite);
  st.Size:=0;
  for i:=0 to verts do begin
    b:=chunkVertexBuffer[i];
    st.WriteBuffer(b,sizeof(b));
  end;
 finally
  st.free;
 end;
end;


{ TTerrainGenerator }

procedure TTerrainGenerator.aoPass(chunk: pChunk);
var
  pBlok: pBlockType;
  x,y,z,i,h,ai: Integer;
begin
 log('TTerrainGenerator.aoPass:: wat?');
 h:=chunk_width*chunk_width;
 for y:=0 to chunk_height-1 do
  for z:=0 to chunk_width-1 do
   for x:=0 to chunk_width-1 do
      begin
         pBlok := Chunk^.pblocks[x, y, z];
         ai:=x+(z*chunk_width)+y*h;
         //if pBlok in mineralsNonBlocking then aoTable[ai];
      end;
end;

constructor TTerrainGenerator.create;
begin
  lastCulledChunk:=0;
  loadBlockColors(appPath+'terrain\blockColorDefs.palette');
  //for optimum performance data should be stored in a format that can be directly read
  //into chunk buffer ( or rather some bigger cache so vbos can be filled without too much
  //waiting. maybe chunks shouldn't have data buffer but rather read from same big
  //common cache and just have pointer to location

  // swiat powinien być w pliku i streamowany

  perlin3DNoise:=TPerlin3DNoise.Create(1234);
  if not gameMode then atmosphere:=TfluidThread.create(False);
end;

destructor TTerrainGenerator.destroy;
begin
// fileStream.free;
  if atmosphere<>nil then begin
    atmosphere.pause;
    atmosphere.terminate;
    atmosphere.waitfor;
    atmosphere.free;
  end;
  perlin3DNoise.destroy;
  if bmpBuffer<>nil then bmpBuffer.free;
  inherited;
end;

procedure TTerrainGenerator.setMapSize;
var i,j:integer;
begin
 lastCulledChunk:=0;
 setlength(cullChunksQueue,active_chunks_w,active_chunks_h);
 for i:=0 to length(cullChunksQueue)-1 do for j:=0 to length(cullChunksQueue)-1 do
    cullChunksQueue[i,j]:=false;
 setlength(topFaces,chunk_width);
 setlength(frontFaces,chunk_width);
 setlength(backFaces,chunk_width);
   //set ao table sie
   //if options.renderer.ambientOcclusion then setlength(aoTable,chunk_width+2+chunk_height+2+chunk_width+2);
end;



procedure TTerrainGenerator.perlin(w,h:integer);
var x,y,z   : integer;
begin
  if bmpBuffer=nil then bmpBuffer:=tbitmap.create;
  with bmpBuffer do
     begin
     width :=w;
     height:=h;
     end;
 with bmpBuffer.canvas do
  for y:=0 to w-1 do
    for x:=0 to h-1 do
             begin
             z:=trunc(abs(PerlinNoise_2D(x /32,y /32))*255);
             pixels[x,y]:=RGBToColor(z,z,z);
             end;
  bmpBuffer.SaveToFile(appPath+'terrain\test.bmp');
end;

procedure TTerrainGenerator.loadFromImage(FileName: string);
begin
 if bmpBuffer=nil then bmpBuffer:=tbitmap.create;
 bmpBuffer.LoadFromFile(appPath+'terrain\'+FileName);
end;

procedure TTerrainGenerator.freeBufferBmp;
begin
 if bmpBuffer<>nil then begin
  bmpBuffer.free;
  bmpBuffer:=nil;
 end;
end;

procedure TTerrainGenerator.saveSector;
begin
  log('TTerrainGenerator.saveSector:: not implemented');
end;

procedure TTerrainGenerator.loadBlockColors(fiile: string);
var
  s: string;
  i:TBlockType;
  ini:tinifile;
begin
  if not fileexists(fiile) then begin
     //when file doesn't exist then load default palette
     fiile:=format('%sblockColorDefs.palette',[ExtractFilePath(fiile)]);
  end;
  ini:=tinifile.Create(fiile);
  for i:=TBlockType(btNone)+2 to TBlockType(btMax)-1 do begin
    s := GetEnumName(TypeInfo(eBlockTypes),TBlockType(i));
    s:=ini.ReadString('colors',s,'0;0;0;1;');
    blockColors[eBlockTypes(i)][0]:=strtofloat(eatstring(s));
    blockColors[eBlockTypes(i)][1]:=strtofloat(eatstring(s));
    blockColors[eBlockTypes(i)][2]:=strtofloat(eatstring(s));
    blockColors[eBlockTypes(i)][3]:=strtofloat(eatstring(s));
  end;
  ini.free;
end;

procedure TTerrainGenerator.saveBlockColors(fiile: string);
var
    s,c: string;
    i:TBlockType;
    ini:tinifile;
begin
  ini:=tinifile.Create(fiile);
  for i:=TBlockType(btNone)+2 to TBlockType(btMax)-1 do begin
    s := GetEnumName(TypeInfo(eBlockTypes),TBlockType(i));
    //check it sprawdz to
    c:= format('%.3f;%.3f;%.3f;%.3f;',[
      blockColors[eBlockTypes(i)][0],
      blockColors[eBlockTypes(i)][1],
      blockColors[eBlockTypes(i)][2],
      blockColors[eBlockTypes(i)][3]
      ]);
    ini.writeString('colors',s,c);
  end;
  ini.free;
end;

procedure TTerrainGenerator.CpyFromBmpBufferToChunkBuffer(blocks: PBlocksBuffer;chunkX,chunkZ:integer);
var
  i,x,y,z:integer;
begin
 //actually y is z in opengl point of view but whatever
  for z:=0 to chunk_width-1 do
    for x:=0 to chunk_width-1 do  begin
         //first zero
        for i:=0 to chunk_height-1 do blocks^[x,i,z]:=byte(eBlockTypes.btNone);
        y:=red(bmpBuffer.canvas.pixels[x+(chunkX*chunk_width),z+(chunkZ*chunk_width)]);
        y:=y div 8;
        blocks^[x,y,z]:=byte(eBlockTypes.btStone);
    end;
 // log('terrain map copied from bmp to chunk');
end;

function packNormal(x,y,z:smallint):single;
begin
 result:= (x+2)*100+(y+2)*10+(z+2);
end;

procedure unpackNormal(a:single;var x,y,z:smallint);
var i:integer;
begin
  i:=round(a);
  x:= (i div 100)-2;
  y:= ((i div 10) mod 10)-2;
  z:= (i mod 10)-2;
end;

//sets vert properties and inc buffer position
procedure addChunkVert(ix,iy,iz,r,g,b,a:single);
begin
  //calculate baked light from surrounding corners
 {
  if options.renderer.ambientOcclusion then begin
  end;
 }
  chunkVertexBuffer[bufPosition].a:=a;
  chunkVertexBuffer[bufPosition].x:=ix;
  chunkVertexBuffer[bufPosition].y:=iy;
  chunkVertexBuffer[bufPosition].z:=iz;
  chunkVertexBuffer[bufPosition].w:=1;
  //colour from blocks definition get's a bit tinted by altitude:
  chunkVertexBuffer[bufPosition].r:=r;//+(iy/50);
  chunkVertexBuffer[bufPosition].g:=g;//+(iy/50);
  chunkVertexBuffer[bufPosition].b:=b;//(random(2)+90)/100;//blockColors[block][2]+(iy/50);
  inc(bufPosition);
end;

procedure addRightFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
  ao,r2,g2,b2,r1,g1,b1,r3,b3,g3,r4,g4,b4:single;
begin
 r*=lum;
 g*=lum;
 b*=lum;
 ao:=0.0;
 with options.renderer do begin
 if sE in sasiady^ then ao:=ao+aoStep;
 if sSE in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 r1:=r-ao; g1:=g-ao; b1:=b-ao;
 addChunkVert(x+0.5,y-0.5,z-0.5,r1,g1,b1,a);

 ao:=0;
 if sN in sasiady^ then ao:=ao+aoStep;
 if sNE in sasiady^ then ao:=ao+aoStep;
 if sE in sasiady^ then ao:=ao+aoStep;
 r2:=r-ao;g2:=g-ao;b2:=b-ao;
 addChunkVert(x+0.5,y+0.5,z-0.5,r2,g2,b2,a);

 ao:=0;
 if sW in sasiady^ then ao:=ao+aoStep;
 if sSW in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 r3:=r-ao;g3:=g-ao;b3:=b-ao;
 addChunkVert(x+0.5,y-0.5,z+0.5,r3,g3,b3,a);

 addChunkVert(x+0.5,y-0.5,z+0.5,r3,g3,b3,a);
 addChunkVert(x+0.5,y+0.5,z-0.5,r2,g2,b2,a);
 ao:=0;
 if sNW in sasiady^ then ao:=ao+aoStep;
 if sN in sasiady^ then ao:=ao+aoStep;
 if sW in sasiady^ then ao:=ao+aoStep;
 r4:=r-ao;g4:=g-ao;b4:=b-ao;
  //if snw in sasiady^ then g4:=1;
 addChunkVert(x+0.5,y+0.5,z+0.5,r4,g4,b4,a);
 end;
end;

procedure addLeftFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
  ao,r2,g2,b2,r1,g1,b1,r3,b3,g3,r4,g4,b4:single;
begin
  r*=lum;
  g*=lum;
  b*=lum;
  ao:=0.0;
 with options.renderer do begin
  if sW in sasiady^ then ao:=ao+aoStep;
  if sNW in sasiady^ then ao:=ao+aoStep;
  if sN in sasiady^ then ao:=ao+aoStep;
  r1:=r-ao; g1:=g-ao; b1:=b-ao;
  addChunkVert(x-0.5,y+0.5,z-0.5,r1,g1,b1,a);

  ao:=0;
  if sW in sasiady^ then ao:=ao+aoStep;
  if sSW in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  r2:=r-ao;g2:=g-ao;b2:=b-ao;
  addChunkVert(x-0.5,y-0.5,z-0.5,r2,g2,b2,a);

  ao:=0;
  if sE in sasiady^ then ao:=ao+aoStep;
  if sSE in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  r3:=r-ao;g3:=g-ao;b3:=b-ao;
  addChunkVert(x-0.5,y-0.5,z+0.5,r3,g3,b3,a);
  addChunkVert(x-0.5,y-0.5,z+0.5,r3,g3,b3,a);

  ao:=0;
  if sNE in sasiady^ then ao:=ao+aoStep;
  if sN in sasiady^ then ao:=ao+aoStep;
  if sE in sasiady^ then ao:=ao+aoStep;
  r4:=r-ao;g4:=g-ao;b4:=b-ao;
  //if sN in sasiady^ then g4:=1;
  addChunkVert(x-0.5,y+0.5,z+0.5,r4,g4,b4,a);
  addChunkVert(x-0.5,y+0.5,z-0.5,r1,g1,b1,a);

 end;
end;

procedure addBottomFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
    r1,g1,b1,r2,g2,b2,ao,r3,g3,b3,r4,g4,b4:single;
begin
  r*=lum;
  g*=lum;
  b*=lum;

  ao:=0.0;
  //obecnosc elementu oznacza obecnosc bloku. a to oznacza ze trzeba sciemnic rog
  //left back vert
 with options.renderer do begin
  if sW in sasiady^ then ao:=ao+aoStep;
  if sNW in sasiady^ then ao:=ao+aoStep;
  if sN in sasiady^ then ao:=ao+aoStep;
  r1:=r-ao; g1:=g-ao; b1:=b-ao;
  addChunkVert(x-0.5,y-0.5,z-0.5,r1,g1,b1,a);
  ao:=0.0;
  if sE in sasiady^ then ao:=ao+aoStep;
  if sNE in sasiady^ then ao:=ao+aoStep;
  if sN in sasiady^ then ao:=ao+aoStep;
  r3:=r-ao; g3:=g-ao; b3:=b-ao;
  addChunkVert(x+0.5,y-0.5,z-0.5,r3,g3,b3,a);

  ao:=0.0;
  if sE in sasiady^ then ao:=ao+aoStep;
  if sSE in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  r2:=r-ao; g2:=g-ao; b2:=b-ao;
  addChunkVert(x+0.5,y-0.5,z+0.5,r2,g2,b2,a);
  addChunkVert(x-0.5,y-0.5,z-0.5,r1,g1,b1,a);

  addChunkVert(x+0.5,y-0.5,z+0.5,r2,g2,b2,a);
  ao:=0.0;
  if sSW in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  if sW in sasiady^ then ao:=ao+aoStep;
  r4:=r-ao; g4:=g-ao; b4:=b-ao;
   //if sne in sasiady^ then g3:=1;
  addChunkVert(x-0.5,y-0.5,z+0.5,r4,g4,b4,a);
 end;
end;

procedure addFrontFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
    r1,g1,b1,r2,g2,b2,ao,r3,g3,b3,r4,g4,b4:single;
begin
  r*=lum;
  g*=lum;
  b*=lum;

  ao:=0.0;
  //obecnosc elementu oznacza obecnosc bloku. a to oznacza ze trzeba sciemnic rog
 with options.renderer do begin
  if sW in sasiady^ then ao:=ao+aoStep;
  if sSW in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  r1:=r-ao; g1:=g-ao; b1:=b-ao;
  addChunkVert(x-0.5,y-0.5,z-0.5,r1,g1,b1,a);

  ao:=0.0;
  if sE in sasiady^ then ao:=ao+aoStep;
  if sNE in sasiady^ then ao:=ao+aoStep;
  if sN in sasiady^ then ao:=ao+aoStep;
  r3:=r-ao; g3:=g-ao; b3:=b-ao;
  addChunkVert(x+0.5,y+0.5,z-0.5,r3,g3,b3,a);

  ao:=0.0;
  if sE in sasiady^ then ao:=ao+aoStep;
  if sSE in sasiady^ then ao:=ao+aoStep;
  if sS in sasiady^ then ao:=ao+aoStep;
  r2:=r-ao; g2:=g-ao; b2:=b-ao;
  addChunkVert(x+0.5,y-0.5,z-0.5,r2,g2,b2,a);
  addChunkVert(x-0.5,y-0.5,z-0.5,r1,g1,b1,a);

  ao:=0.0;
  if sN in sasiady^ then ao:=ao+aoStep;
  if sNW in sasiady^ then ao:=ao+aoStep;
  if sW in sasiady^ then ao:=ao+aoStep;
  r4:=r-ao; g4:=g-ao; b4:=b-ao;
   //if snw in sasiady^ then g4:=1;
   addChunkVert(x-0.5,y+0.5,z-0.5,r4,g4,b4,a);
   addChunkVert(x+0.5,y+0.5,z-0.5,r3,g3,b3,a);
  end;
end;

procedure addBackFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
  r1,g1,b1,r2,g2,b2,ao,r3,g3,b3,r4,g4,b4:single;
begin
 r*=lum;
 g*=lum;
 b*=lum;

 ao:=0.0;
 //obecnosc elementu oznacza obecnosc bloku. a to oznacza ze trzeba sciemnic rog
 with options.renderer do begin
 if sW in sasiady^ then ao:=ao+aoStep;
 if sSW in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 r1:=r-ao; g1:=g-ao; b1:=b-ao;
 addChunkVert(x-0.5,y-0.5,z+0.5,r1,g1,b1,a);

 ao:=0.0;
 if sE in sasiady^ then ao:=ao+aoStep;
 if sSE in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 r2:=r-ao; g2:=g-ao; b2:=b-ao;
 addChunkVert(x+0.5,y-0.5,z+0.5,r2,g2,b2,a);

 ao:=0.0;
 if sE in sasiady^ then ao:=ao+aoStep;
 if sNE in sasiady^ then ao:=ao+aoStep;
 if sN in sasiady^ then ao:=ao+aoStep;
 r3:=r-ao; g3:=g-ao; b3:=b-ao;
 addChunkVert(x+0.5,y+0.5,z+0.5,r3,g3,b3,a);

 ao:=0.0;
 if sN in sasiady^ then ao:=ao+aoStep;
 if sNW in sasiady^ then ao:=ao+aoStep;
 if sW in sasiady^ then ao:=ao+aoStep;
 r4:=r-ao; g4:=g-ao; b4:=b-ao;
 //if sw in sasiady^ then g4:=1;

  addChunkVert(x-0.5,y-0.5,z+0.5,r1,g1,b1,a);
  addChunkVert(x+0.5,y+0.5,z+0.5,r3,g3,b3,a);
  addChunkVert(x-0.5,y+0.5,z+0.5,r4,g4,b4,a);
 end;
end;

procedure addTopFace(x,y,z:integer;r,g,b,a:single;sasiady:pSasiedzi;lum:single);
var
  r1,g1,b1,r2,g2,b2,ao,r3,g3,b3,r4,g4,b4:single;
begin
 r*=lum;
 g*=lum;
 b*=lum;

 ao:=0.0;
 //obecnosc elementu oznacza obecnosc bloku. a to oznacza ze trzeba sciemnic rog
 //left back vert
 with options.renderer do begin
 if sW in sasiady^ then ao:=ao+aoStep;
 if sNW in sasiady^ then ao:=ao+aoStep;
 if sN in sasiady^ then ao:=ao+aoStep;
 r1:=r-ao; g1:=g-ao; b1:=b-ao;
 addChunkVert(x-0.5,y+0.5,z-0.5,r1,g1,b1,a);

 ao:=0.0;
 if sE in sasiady^ then ao:=ao+aoStep;
 if sSE in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 r2:=r-ao; g2:=g-ao; b2:=b-ao;
 addChunkVert(x+0.5,y+0.5,z+0.5,r2,g2,b2,a);

 ao:=0.0;
 if sE in sasiady^ then ao:=ao+aoStep;
 if sNE in sasiady^ then ao:=ao+aoStep;
 if sN in sasiady^ then ao:=ao+aoStep;
 r3:=r-ao; g3:=g-ao; b3:=b-ao;
 addChunkVert(x+0.5,y+0.5,z-0.5,r3,g3,b3,a);
 addChunkVert(x-0.5,y+0.5,z-0.5,r1,g1,b1,a);

 ao:=0.0;
 if sSW in sasiady^ then ao:=ao+aoStep;
 if sS in sasiady^ then ao:=ao+aoStep;
 if sW in sasiady^ then ao:=ao+aoStep;
 r4:=r-ao; g4:=g-ao; b4:=b-ao;
  //if sne in sasiady^ then g3:=1;
 addChunkVert(x-0.5,y+0.5,z+0.5,r4,g4,b4,a);
 addChunkVert(x+0.5,y+0.5,z+0.5,r2,g2,b2,a);

 end;
end;


//wrzucic ze strszej wersji szybszy cullling na wszellki wypadek
procedure TTerrainGenerator.cullBlocksInChunk(chunks: PChunksArray; chunkX,
  chunkY: integer;executeNow:boolean=false);
var
  offsetxz,xx,yy,zz,x,y,z:integer;
  left,right,front,back,top,btm,chxL,chxR,chzB,chzF:integer;
  block :eBlockTypes;
  rr,gg,bb:single;
  sasiady:TSasiedzi;
begin
  if not executeNow then begin
     cullChunksQueue[chunkX,chunkY]:=true;
     exit;
  end else cullChunksQueue[chunkX,chunkY]:=false;
  chunks^[chunkX,chunkY].reBuild:=false;
  currentChunkOff[0]:=chunkX*chunk_width;
  currentChunkOff[1]:=chunkY*chunk_width;
  {$IFNDEF SERVER}
  //offset for chunk bufPosition alignemt
  offsetxz:=-(chunk_width2 *block_size);
  bufPosition:=0;
  takeCareOfDataBufferSize(256000);
  //first iterate the inner part. just to spare all the ifs for suroounding chunks
  for y:=0 to chunk_height-1 do
   for z:=0 to chunk_width-1 do begin
    for x:=0 to chunk_width-1 do begin

      frontFaces[x]:=btNone;
      backFaces[x]:=btNone;
      //block:=eBlockTypes(core.chunks[chunkX,chunkY].blocks[x,y,z]);
      block:=eBlockTypes(chunksPointer^[chunkX,chunkY].pblocks[x,y,z]^);
      //if block in mineralsWater then
         //atmosphere.waterMap.setWet(chunkX*chunk_width+x,y,chunkY*chunk_width+z);
      //if block in mineralsOxygen then
         //atmosphere.oxygenMap.setWet(chunkX*chunk_width+x,y,chunkY*chunk_width+z);
      if not (block in mineralsDontRender) then
      begin
       //x axis
       left:=-1;
       chXL:=0;
       if (chunkX=0) and (x=0) then left:=0;
       if (chunkX>0) and (x=0) then begin
           left:=chunk_width-1;//styczny z poprzedniego bloku
           chXL:=-1;
       end;
       right:=1;
       chxR:=0;
       if (chunkX<active_chunks_w-1) and (x=chunk_width-1) then begin
           right:=-chunk_width+1;//sasiad z nastepnego bloku
           chxR:=1;
       end;
       if (chunkX=active_chunks_w-1) and (x=chunk_width-1) then begin
           right:=0;
       end;
       //z axis
       front:=-1;
       chZF:=0;
       if (chunkY=0) and (z=0) then front:=0;
       if (chunkY>0) and (z=0) then begin
           front:=chunk_width-1;//styczny z poprzedniego bloku
           chZF:=-1;
       end;
       back:=1;
       chzB:=0;
       if (chunkY<active_chunks_h-1) and (z=chunk_width-1) then begin
           back:=-chunk_width+1;//sasiad z nastepnego bloku
           chzB:=1;
       end;
       if (chunkY=active_chunks_h-1) and (z=chunk_width-1) then begin
           back:=0;
       end;
       top:=1;
       if (y=chunk_height-1) or (y=0) then begin
          top:=0;
          // a:=a+16;
       end;
       btm:=1;
       if y=0 then btm:=0;
        xx:=offsetxz+x;
        yy:=y;
        zz:=offsetxz+z;
        //colour from blocks definition get's a bit tinted by altitude:
        rr:=blockColors[block][0];//+(y/50);
        gg:=blockColors[block][1];//+(y/50);
        bb:=blockColors[block][2];//(random(2)+90)/100;//blockColors[block][2]+(y/50);
        sasiady:=[];
        //cull here first, maybe block won't be shown at all
        if eBlockTypes(chunks^[chunkX+chXL,chunkY].blocks[x+left,y,z]) in mineralsDontRender then //left face
        begin
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y,z+front]) in mineralsDontRender)
             then include(sasiady,sW) else exclude(sasiady,sW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZB].blocks[x+left,y,z+back])in mineralsDontRender)
             then include(sasiady,sE) else exclude(sasiady,sE);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY].blocks[x+left,y+top,z])in mineralsDontRender)
             then include(sasiady,sN) else exclude(sasiady,sN);
          if not (eBlockTypes(chunks^[chunkX+chxL,chunkY].blocks[x+left,y-top,z]) in mineralsDontRender)
             then include(sasiady,sS) else exclude(sasiady,sS);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y+top,z+front]) in mineralsDontRender)
             then include(sasiady,sNW) else exclude(sasiady,sNW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y-top,z+front]) in mineralsDontRender)
             then include(sasiady,sSW) else exclude(sasiady,sSW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y+top,z+back]) in mineralsDontRender)
             then include(sasiady,sNE) else exclude(sasiady,sNE);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y-top,z+back]) in mineralsDontRender)
             then include(sasiady,sSE) else exclude(sasiady,sSE);
           addLeftFace(xx,yy,zz,rr,gg,bb,packNormal(-1,0,0),@sasiady,
                       //1
                       calculateAO((chunkX+chxL)*chunk_width+x+left,y,chunkY*chunk_width+z,fLeft)
                       );
        end;
        if eBlockTypes(chunks^[chunkX+chXR,chunkY].blocks[x+right,y,z]) in mineralsDontRender then //right face
        begin
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZb].blocks[x+right,y,z+back]) in mineralsDontRender)
             then include(sasiady,sW) else exclude(sasiady,sW);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZf].blocks[x+right,y,z+front])in mineralsDontRender)
             then include(sasiady,sE) else exclude(sasiady,sE);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY].blocks[x+right,y+top,z])in mineralsDontRender)
             then include(sasiady,sN) else exclude(sasiady,sN);
          if not (eBlockTypes(chunks^[chunkX+chxr,chunkY].blocks[x+right,y-top,z]) in mineralsDontRender)
             then include(sasiady,sS) else exclude(sasiady,sS);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZb].blocks[x+right,y+top,z+back]) in mineralsDontRender)
             then include(sasiady,sNW) else exclude(sasiady,sNW);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZb].blocks[x+right,y-top,z+back]) in mineralsDontRender)
             then include(sasiady,sSW) else exclude(sasiady,sSW);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZf].blocks[x+right,y+top,z+front]) in mineralsDontRender)
             then include(sasiady,sNE) else exclude(sasiady,sNE);
          if not (eBlockTypes(chunks^[chunkX+chXr,chunkY+chZf].blocks[x+right,y-top,z+front]) in mineralsDontRender)
             then include(sasiady,sSE) else exclude(sasiady,sSE);
          addRightFace(xx,yy,zz,rr,gg,bb,packNormal(1,0,0),@sasiady,
                       calculateAO((chunkX+chxr)*chunk_width+x+right,y,chunkY*chunk_width+z,fright));
        end;
        if eBlockTypes(chunks^[chunkX,chunkY+chZF].blocks[x,y,z+front])  in mineralsDontRender then begin//front face
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chzf].blocks[x+left,y,z+front]) in mineralsDontRender)
             then include(sasiady,sW) else exclude(sasiady,sW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chzf].blocks[x+right,y,z+front])in mineralsDontRender)
             then include(sasiady,sE) else exclude(sasiady,sE);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZf].blocks[x,y+top,z+front])in mineralsDontRender)
             then include(sasiady,sN) else exclude(sasiady,sN);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZf].blocks[x,y-top,z+front]) in mineralsDontRender)
             then include(sasiady,sS) else exclude(sasiady,sS);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y+top,z+front]) in mineralsDontRender)
             then include(sasiady,sNW) else exclude(sasiady,sNW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y-top,z+front]) in mineralsDontRender)
             then include(sasiady,sSW) else exclude(sasiady,sSW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZf].blocks[x+right,y+top,z+front]) in mineralsDontRender)
             then include(sasiady,sNE) else exclude(sasiady,sNE);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZf].blocks[x+right,y-top,z+front]) in mineralsDontRender)
             then include(sasiady,sSE) else exclude(sasiady,sSE);
         addFrontFace(xx,yy,zz,rr,gg,bb,packNormal(0,0,1),@sasiady,
                                calculateAO((chunkX)*chunk_width+x,y,(chunkY+chzf)*chunk_width+z+front,fFRont));
        end;
        if eBlockTypes(chunks^[chunkX,chunkY+chZB].blocks[x,y,z+back]) in mineralsDontRender then begin//beck? face
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chzb].blocks[x+left,y,z+back]) in mineralsDontRender)
             then include(sasiady,sW) else exclude(sasiady,sW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chzb].blocks[x+right,y,z+back])in mineralsDontRender)
             then include(sasiady,sE) else exclude(sasiady,sE);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZb].blocks[x,y+top,z+back])in mineralsDontRender)
             then include(sasiady,sN) else exclude(sasiady,sN);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZb].blocks[x,y-top,z+back]) in mineralsDontRender)
             then include(sasiady,sS) else exclude(sasiady,sS);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y+top,z+back]) in mineralsDontRender)
             then include(sasiady,sNW) else exclude(sasiady,sNW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y-top,z+back]) in mineralsDontRender)
             then include(sasiady,sSW) else exclude(sasiady,sSW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZb].blocks[x+right,y+top,z+back]) in mineralsDontRender)
             then include(sasiady,sNE) else exclude(sasiady,sNE);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZb].blocks[x+right,y-top,z+back]) in mineralsDontRender)
             then include(sasiady,sSE) else exclude(sasiady,sSE);
           addBackFace(xx,yy,zz,rr,gg,bb,packNormal(0,0,-1),@sasiady,
                       calculateAO((chunkX)*chunk_width+x,y,(chunkY+chzb)*chunk_width+z+back,fback));
        end;

        if eBlockTypes(chunks^[chunkX,chunkY].blocks[x,y+top,z]) in mineralsDontRender then   begin//top face
           if not (eBlockTypes(chunks^[chunkX+chXL,chunkY].blocks[x+left,y+top,z]) in mineralsDontRender)
              then include(sasiady,sW) else exclude(sasiady,sW);
           if not (eBlockTypes(chunks^[chunkX+chXR,chunkY].blocks[x+right,y+top,z])in mineralsDontRender)
              then include(sasiady,sE) else exclude(sasiady,sE);
           if not (eBlockTypes(chunks^[chunkX,chunkY+chZf].blocks[x,y+top,z+front])in mineralsDontRender)
              then include(sasiady,sN) else exclude(sasiady,sN);
           if not (eBlockTypes(chunks^[chunkX,chunkY+chZb].blocks[x,y+top,z+back]) in mineralsDontRender)
              then include(sasiady,sS) else exclude(sasiady,sS);
           if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y+top,z+front]) in mineralsDontRender)
              then include(sasiady,sNW) else exclude(sasiady,sNW);
           if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y+top,z+back]) in mineralsDontRender)
              then include(sasiady,sSW) else exclude(sasiady,sSW);
           if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZf].blocks[x+right,y+top,z+front]) in mineralsDontRender)
              then include(sasiady,sNE) else exclude(sasiady,sNE);
           if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZb].blocks[x+right,y+top,z+back]) in mineralsDontRender)
              then include(sasiady,sSE) else exclude(sasiady,sSE);
           addTopFace(xx,yy,zz,rr,gg,bb,packNormal(0,1,0),@sasiady,
                        calculateAO(chunkX*chunk_width+x,y+top,chunkY*chunk_width+z,fTop));
           end;
           //addTopFace(xx,yy,zz,1,rr,gg,bb,5.0);
        if eBlockTypes(chunks^[chunkX,chunkY].blocks[x,y-btm,z]) in mineralsDontRender then  //btm face
        begin
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY].blocks[x+left,y-btm,z]) in mineralsDontRender)
             then include(sasiady,sW) else exclude(sasiady,sW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY].blocks[x+right,y-btm,z])in mineralsDontRender)
             then include(sasiady,sE) else exclude(sasiady,sE);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZf].blocks[x,y-btm,z+front])in mineralsDontRender)
             then include(sasiady,sN) else exclude(sasiady,sN);
          if not (eBlockTypes(chunks^[chunkX,chunkY+chZb].blocks[x,y-btm,z+back]) in mineralsDontRender)
             then include(sasiady,sS) else exclude(sasiady,sS);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZf].blocks[x+left,y-btm,z+front]) in mineralsDontRender)
             then include(sasiady,sNW) else exclude(sasiady,sNW);
          if not (eBlockTypes(chunks^[chunkX+chXL,chunkY+chZb].blocks[x+left,y-btm,z+back]) in mineralsDontRender)
             then include(sasiady,sSW) else exclude(sasiady,sSW);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZf].blocks[x+right,y-btm,z+front]) in mineralsDontRender)
             then include(sasiady,sNE) else exclude(sasiady,sNE);
          if not (eBlockTypes(chunks^[chunkX+chXR,chunkY+chZb].blocks[x+right,y-btm,z+back]) in mineralsDontRender)
             then include(sasiady,sSE) else exclude(sasiady,sSE);
           addBottomFace(xx,yy,zz,rr,gg,bb,packNormal(0,-1,0),@sasiady,
                          //1);
                        calculateAO(chunkX*chunk_width+x,y-btm,chunkY*chunk_width+z,fBottom));
        end;
       end;
    end;
    //addFrontFaces(offsetxz,offsety+y,offsetxz+z);
    //addTopFaces(offsetxz,offsety+y,offsetxz+z);
    //addBackFaces(offsetxz,offsety+y,offsetxz+z);
   end;
  vboDumpCount:=(bufPosition*sizeof(rBufferDataBit));
  chunksPointer^[chunkX,chunkY].renderable.updateVBO(@chunkVertexBuffer,vboDumpCount);
  //core.chunks[chunkX,chunkY].renderable.updateVBO(@chunkVertexBuffer,vboDumpCount);
  {$ENDIF}
end;

procedure TTerrainGenerator.cullAllChunks;
var x,z:integer;
begin
 for z:=0 to active_chunks_h-1 do
   for x:=0 to active_chunks_w-1 do
     cullBlocksInChunk(chunksPointer,x,z);
end;

function TTerrainGenerator.calculateAO(xx, yy, zz:integer; dir: eFaces): single;
var i:integer;
    total:integer;
begin
 total:=0;
 with options.renderer do begin//shortcut for options.renderer.aoRayLength
 case dir of
  fTop:begin
    for i:=0 to aoRayLength-1 do //shoot ray up
      if getWorldArray(xx,yy+i,zz) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx+i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx-i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx+i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx-i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
  end;
  fLeft:begin
    for i:=0 to aoRayLength-1 do //shoot ray left
        if getWorldArray(xx-i,yy,zz) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
  end;
  fRight:begin
    for i:=0 to aoRayLength-1 do //shoot ray left
        if getWorldArray(xx+i,yy,zz) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
  end;
  fBack:begin
    for i:=0 to aoRayLength-1 do //shoot ray left
        if getWorldArray(xx,yy,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz+i) in mineralsDontRender then inc(total) else break;
  end;
  ffront:begin
    for i:=0 to aoRayLength-1 do //shoot ray left
        if getWorldArray(xx,yy,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx+i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
        if getWorldArray(xx-i,yy+i,zz-i) in mineralsDontRender then inc(total) else break;
  end;
  fBottom:begin
    for i:=0 to aoRayLength-1 do //shoot ray up
      if getWorldArray(xx,yy-i,zz) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx+i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx-i,yy-i,zz+i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx+i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
    for i:=0 to aoSkosRayLength-1 do
      if getWorldArray(xx-i,yy-i,zz-i) in mineralsDontRender then inc(total) else break;
  end;
  end;
  //more light means less to substract
  result:= total / (aoRayLength + 4*aoSkosRayLength);
  end;
  if result = 0 then result:=0.2
   //log(floattostr(result));
end;

procedure addTopFaceF(x,y,z,r,g,b,a:single);
begin
 terrain.colorOscilator(r,g,b);
 addChunkVert(x-0.5,y+0.5,z-0.5,r,g,b,a);
 addChunkVert(x+0.5,y+0.5,z+0.5,r,g,b,a);
 addChunkVert(x+0.5,y+0.5,z-0.5,r,g,b,a);
 addChunkVert(x-0.5,y+0.5,z-0.5,r,g,b,a);
 addChunkVert(x-0.5,y+0.5,z+0.5,r,g,b,a);
 addChunkVert(x+0.5,y+0.5,z+0.5,r,g,b,a);
end;

procedure addBackFaceF(x,y,z,r,g,b,a:single);
begin
  terrain.colorOscilator(r,g,b);
  addChunkVert(x-0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y+0.5,z+0.5,r,g,b,a);
end;

procedure addFrontFaceF(x,y,z,r,g,b,a:single);
begin
  terrain.colorOscilator(r,g,b);
  addChunkVert(x-0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x-0.5,y+0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z-0.5,r,g,b,a);
end;

procedure addRightFaceF(x,y,z,r,g,b,a:single);
begin
  terrain.colorOscilator(r,g,b);
  addChunkVert(x+0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y+0.5,z+0.5,r,g,b,a);
end;

procedure addLeftFaceF(x,y,z,r,g,b,a:single);
begin
  terrain.colorOscilator(r,g,b);
  addChunkVert(x-0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y+0.5,z-0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y+0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y+0.5,z-0.5,r,g,b,a);
end;

procedure addBottomFaceF(x,y,z,r,g,b,a:single);
begin
  terrain.colorOscilator(r,g,b);
  addChunkVert(x-0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z-0.5,r,g,b,a);
  addChunkVert(x+0.5,y-0.5,z+0.5,r,g,b,a);
  addChunkVert(x-0.5,y-0.5,z+0.5,r,g,b,a);
end;


procedure TTerrainGenerator.setBlockAndCull(pos: TAffineVector;
  blokType: eBlockTypes);
var chX,chY:integer;
    blokPos:TVector3b;
begin
  chX:=round(pos[0]+world_width2) div( chunk_width);
  chY:=round(pos[2]+world_depth2) div ( chunk_width);
  if chX>=active_chunks_w then chX:=active_chunks_w-1;
  if chY>=active_chunks_h then chY:=active_chunks_h-1;
  blokPos:=vector3bmake(abs(round(pos[0]+world_width2)) mod (chunk_width),
          round(pos[1]),
          abs(round(pos[2]+world_depth2)) mod (chunk_width));
  chunksPointer^[chx,chy].setBlock(blokPos,blokType);
  cullChunksQueue[chX,chY]:=true;
  //cull edge chunks if necessary
  if (blokPos[0]=0) and (chx>0) then cullChunksQueue[chx-1,chy]:=true;
  if (blokPos[0]=chunk_width-1) and (chx<active_chunks_w-1) then cullChunksQueue[chx+1,chy]:=true;
  if (blokPos[1]=0) and (chy>0) then cullChunksQueue[chx,chy-1]:=true;
  if (blokPos[1]=chunk_width-1) and (chy<active_chunks_h-1) then cullChunksQueue[chx,chy+1]:=true;
end;

procedure TTerrainGenerator.setBlockInWorld(x, y, z: integer;
  blok: eBlockTypes; centered,cull: boolean);
var chX,chY:integer;
    blokPos:TVector3b;
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
    if cull then cullBlocksInChunk(chunksPointer,chX,chY,false);
    //blok:=btNone;
  end else begin
    chunksPointer^[x div chunk_width,z div chunk_width].blocks[x mod chunk_width,y,z mod chunk_width]:=byte(blok);
  end;
end;

function TTerrainGenerator.getBlockInWorld(x, y, z: integer; centered: boolean
  ): pBlockType;
var chX,chY:integer;
    blokPos:TVector3b;
begin
  if centered then begin
    chX:=round(x+world_width2) div( chunk_width);
    chY:=round(z+world_depth2) div ( chunk_width);
    blokPos:=vector3bmake(abs(round(x+world_width2)) mod (chunk_width),
           round(y),
           abs(round(z+world_depth2)) mod (chunk_width));
    result:=chunksPointer^[chx,chy].pblocks[blokPos[0],blokPos[1],blokPos[2]];
  end
  else begin
    result:=chunksPointer^[x div chunk_width,z div chunk_width].pblocks[x mod chunk_width,y,z mod chunk_width];
  end;
end;

end.

